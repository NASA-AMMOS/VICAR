#!/usr/bin/env python
'''
Author: Aaron Roth
Summary:
Update make_orbital_mesh.csh to run easier and to be able to be resized.
'''
import shutil
import os
import subprocess
from subprocess import PIPE
import glob
import sys
import configparser as cfg

'''
Oleg:
His whole goal is to simplify this script as much as possible and make it
able to take any DEM and output any mesh or obj we want

Right now is to take out as much hardcoded things in the code that make it
specific. we want to make this a universal tool.

First take out the extra specific places call and allow it to take in an
environmental variable. Oleg also mentioned for this step that it
should take in a DEM file

Ask Oleg how to get the origin meshing position from a DEM input file. He said
for now just hardcode a value for testing reasons
'''


PY2 = False
if sys.version_info[0] < 3:
    PY2 = True
# ***COMMENTED OUT FOR TESTING***
# PLACES = os.environ["PLACES_BASE_URL"]
PLACES = "https://places.sops.m20.jpl.nasa.gov/"


def places_curl_cmder(num, site, drive, work_dir, site_flag=False):
    '''
    Summary:
    Look through places and find x and y values for certain orbital sites

    Note. Turn places https into a variable or a parameter file with name
    value pairs. Or use the environment variable on system but you also have to
    add support for an input file with the pos

    Arguments:
    num - orbital site number. each number correlates to either DEM, color, etc
    site - rover site
    drive - rover drive
    work_dir - work dir is the tmp directory to create resulting txt files
    site_flag - Boolean determining whether to use orbital site number or site.
    '''

    text_name = "orb" + str(num)
    #local_tel = "localized_pos"
    local_tel = "best_tactical"
    orb_site = "ORBITAL"
    if site_flag:
        num = site
        text_name = "site"
        local_tel = "telemetry"
        orb_site = "SITE"

    places_url = (
        PLACES + "/query/primary/" +
        local_tel + "?from=ROVER(" + site + "," + drive + ")&to=" + orb_site +
        "(" + str(num) + ")\" > ")

    curl_cmd = (
        "curl -L -b \"ssosession=`cat ~/.cssotoken/ssosession`\" \"" + places_url +
        work_dir + "rover_" + text_name + ".txt")
    print("curl_cmd: ", curl_cmd)
    subprocess.call(curl_cmd, shell=True)

def pos_finder(num, work_dir, site_flag=False):
    '''
    Summary:
    Find rover position in relation to an orbital map site by parsing txt file

    Arguments:
    num - orbital site number
    work_dir - temporary dir in which rover position txt files are saved.
    site_flag - boolean to use "site" instead of an orbital site number
    '''
    text_name = "orb" + str(num) + ".txt"
    if site_flag:
        text_name = "site.txt"
    doc = open(work_dir + "rover_" + text_name)
    print(work_dir + "rover_" + text_name)
    '''
    find x, y, z value and return
    '''
    for line in doc.readlines():
        vallist = line.split("\"")
        cnt = 0
        while cnt < len(vallist) :
            if "x=" in vallist[cnt] :
                x_val = vallist[cnt+1]
            if "y=" in vallist[cnt] :
                y_val = vallist[cnt+1]
            if "z=" in vallist[cnt] :
                z_val = vallist[cnt+1]
            cnt += 1
    #return float(val[0]), float(val[1]), float(val[2])
    return float(z_val), float(y_val), float(x_val)


def sl_ss_math(x, y, skin_size, origin_offset_x, origin_offset_y,
               tilelist=None, scale=0.25):
    '''
    Summary:
    Calculate the starting line and starting sample of each tile.
    Each tile is 256 pixels wide and long.

    Arguments:
    x - x origin
    y - y origin
    skin_size -
    origin_offset_x
    origin_offset_y
    tilelist - used to grow, shrink, or relocate, the mesh.
    dem - should be named differently. just to signify scale
    '''
    list_sl = []
    list_ss = []

    print("sl_ss_math, x: ", x)

    if scale == 1:
        # DEM maps (or overlays???) have 1m per pixel resolution
        sl_f = -(x - origin_offset_x)
        ss_f = (y - origin_offset_y)
    else:
        # textured maps have 25cm per pixel resolution.
        sl_f = -(x - origin_offset_x) / scale
        ss_f = (y - origin_offset_y) / scale
    str_sl = format(float(sl_f), '06.0f')
    str_ss = format(float(ss_f), '06.0f')
    sl = float(str_sl)
    ss = float(str_ss)
    skin_size = int(skin_size)
    low_range_sl = -1
    hi_range_sl = 3
    low_range_ss = -1
    hi_range_ss = 2
    north = 0
    south = 0
    east = 0
    west = 0

    # grow baby grow
    if tilelist:
        north = tilelist[0]
        east = tilelist[1]
        south = tilelist[2]
        west = tilelist[3]

    if north != 0:
        low_range_sl = low_range_sl - north
    if south != 0:
        hi_range_sl = hi_range_sl + south
    if west != 0:
        low_range_ss = low_range_ss - west
    if east != 0:
        hi_range_ss = hi_range_ss + east

    if low_range_sl > hi_range_sl:
        raise Exception("North is more north than South." +
                        " Change grow dimensions")

    if low_range_ss > hi_range_ss:
        raise Exception("West is more East than East. Change grow dimensions")

    for j in range(low_range_sl, hi_range_sl + 1):
        # resu = 256 * j - 255
        add_sl = sl + (j - 1) * (skin_size - 1)
        list_sl.append(format(float(add_sl), '06.0f'))

    for k in range(low_range_ss, hi_range_ss + 1):
        add_ss = ss + k * (skin_size - 1)
        list_ss.append(format(float(add_ss), '06.0f'))

    list_sl.sort()
    list_ss.sort()

    return list_sl, list_ss


def R2LIB_copy(inp, out_dir, out_prod, list_sl, list_ss, s_size, d_size,
               demlist_sl, demlist_ss):
    pad_s_size = '{:06d}'.format(int(s_size))
    pad_s_size = str(pad_s_size)
    s_size = str(s_size)
    d_size = str(d_size)
    for sl, dl in zip(list_sl, demlist_sl):
        for ss, ds in zip(list_ss, demlist_ss):
            r2lib_cmd = (
                os.environ["R2LIB"] + "/copy " + inp + " " + out_dir +
                "/ORB_000000_0000_" + out_prod + "_L" + sl + "_S" + ss + "_" +
                pad_s_size + "_" + pad_s_size + ".VIC sl=" + dl + " ss=" + ds +
                " nl=" + d_size + " ns=" + d_size)
            print("============================================" +
                  "======================================")
            print(r2lib_cmd)
            subprocess.call(r2lib_cmd, shell=True)


def make_mesh_call(prod_dir, out_prod, demlist_sl, demlist_ss, list_sl,
                   list_ss, d_size, s_size, dem_scale, dem_val, demskin,
                   tilelist=None):
    # setting default parameter dimensions of mesh.
    low_range_sl = -1
    hi_range_sl = 3
    low_range_ss = -1
    hi_range_ss = 2

    north = 0
    east = 0
    south = 0
    west = 0
    sl_offset = 0
    ss_offset = 0
    # grow baby grow
    if tilelist:
        north = tilelist[0]
        east = tilelist[1]
        south = tilelist[2]
        west = tilelist[3]

    if north != 0:
        low_range_sl = low_range_sl - north
        sl_offset -= north
    if south != 0:
        hi_range_sl = hi_range_sl + south
        sl_offset += south
    if west != 0:
        low_range_ss = low_range_ss - west
        ss_offset -= west
    if east != 0:
        hi_range_ss = hi_range_ss + east
        ss_offset += east

    pad_d_size = '{:06d}'.format(int(d_size))
    pad_s_size = '{:06d}'.format(int(s_size))
    dem_scale = str(dem_scale)
    dem_val = str(dem_val)
    sl_count = 0
    ss_count = 0
    fparm = ''  # correlates with first parm. gotta change this
    sparm = ''  # correlates with second parm. gotta change this to more detail
    tru_d_size = int(d_size) - 1
    d_size = str(d_size)
    s_size = str(s_size)

    for d_sl, sl in zip(demlist_sl, list_sl):
        ss_count = 0
        # Setting the line of the tile we're adding
        j = hi_range_sl - sl_count - sl_offset
        origin_line = tru_d_size * j - (tru_d_size - 1)
        fparm = str(origin_line)

        for d_ss, ss in zip(demlist_ss, list_ss):
            # setting the sample of the tile we're adding
            k = hi_range_ss - ss_count - ss_offset
            origin_sample = tru_d_size * k - (tru_d_size - 1)
            sparm = str(origin_sample)

            make_mesh_cmd = (
                #"make_mesh.sh " + prod_dir + "ORB_000000_0000_DEM_L" + d_sl +
                os.environ["CRUMBS_HOME"] + "/orbital/make_mesh.sh " + prod_dir + "ORB_000000_0000_DEM_L" + d_sl +
                "_S" + d_ss + "_" + pad_d_size + "_" + pad_d_size + ".VIC " +
                prod_dir + "ORB_000000_0000_" + out_prod + "_L" + sl +
                "_S" + ss + "_" + pad_s_size + "_" + pad_s_size + ".VIC " +
                dem_scale + " " + dem_val + " " + prod_dir + "oss " + fparm +
                " " + sparm + " " + d_size + " " + s_size + " 1 " + demskin)

            print("\n======================================================" +
                  "============================")
            print(make_mesh_cmd)
            subprocess.call(make_mesh_cmd, shell=True)
            ss_count += 1
        sl_count += 1


def merge_mesh_call(work_dir, skin, sol, site, drive, location_dir=None):
    C = "G"
    if skin == "BSM":
        C = "C"
    meshlist = glob.glob(
        work_dir + "oss/mesh/ORB_000000_0000_" + skin + "*.iv")
    print(work_dir + "oss/mesh/ORB_000000_0000_" + skin + "*.iv")
    meshlist.sort()
    listname = (
        "O_AT_" + sol + "_ORR_S10TRAS" + site + "_" + drive +
        "_" + C + "_BSM25J01.LIS")
    listfile = (work_dir + listname)
    f = open(listfile, "w+")
    for i in meshlist:
        # print(i)
        f.write(i + "\n")

    f.close()
    if location_dir:
        out_dir = location_dir
    else:
        out_dir = "/ods/surface/sol/0" + sol + "/opgs/rdr/mesh"
    merge_mesh_cmd = (
        os.environ["CRUMBS_BIN"] + "/merge_mesh " + listfile +
        " " + out_dir)

    print("==========================================================" +
          "========================")
    print(merge_mesh_cmd)
    subprocess.call(merge_mesh_cmd, shell=True)
    bname_no_ext = listname.split('.')[0]
    fin_bname = bname_no_ext + '.iv'
    final_location = out_dir + "/" + fin_bname
    return final_location


def main(solsitedrive, dem_inp='DEM', skin_size=1024, req_type='ods', ovr_list=[],
         bigger=None, outputdir=None, skin_list=['BSM', 'BWP'], o_loc=None):

    '''
    d
    '''

    sol = solsitedrive[0]
    sol = '{:04d}'.format(sol)
    site = solsitedrive[1]
    site = '{:03d}'.format(site)
    drive = solsitedrive[2]
    drive = '{:04d}'.format(drive)
    tilelist = None
    if bigger:
        # better name too lazy to change everything too.
        tilelist = bigger

    output_dir = ''
    if outputdir:
        output_dir = outputdir

    config = cfg.ConfigParser()
    # change to tools/sbin later.
    template_file = os.environ["CRUMBS_HOME"] + "/orbital/dem2mesh_config.cfg"
    #template_file = "/home/ozp/workspace/git4/Vicar_dev/VICAR/crumbs/" + "/orbital/dem2mesh_config.cfg"
    #template_file = "/home/ozp/workspace/git5/MIPL/Vicar_dev/VICAR/crumbs/orbital/dem2mesh_config.cfg"
    print(template_file)
    config.read(template_file)
    print(config.sections())

    overlay_dict = config['Orbital_Overlays']
    if ovr_list:
        if "ORR" not in ovr_list or "orr" not in ovr_list:
            ovr_list.append("ORR")
    scale_dict = config['Skin_Scale']
    texture_dict = config['Textures']
    dem_dict = config['Dem']
    DEM = dem_dict[dem_inp]
    orb_site_dict = config['Orbital_Site']
    overlay_dir_dict = config['Overlay_Dirs']
    overlay_dir = overlay_dir_dict[req_type]
    if output_dir:
        overlay_dir = output_dir + "/../orbital"

    '''
    orbital_dir = "/proj/msl/redops/ods/surface/strategic/opgs/orbital/"

    overlay_dem = orbital_dir + "MSL_Gale_DEM_Mosaic_1m_v3.VIC"

    DEM = orbital_dir + "MSL_Gale_DEM_Mosaic_1m_v3.VIC"
    '''

    # Setting up directories
    work_dir = "/tmp/" + sol + "/"

    if os.path.exists(work_dir):
        shutil.rmtree(work_dir)

    if PY2:
        if not os.path.exists(work_dir):
            os.mkdir(work_dir)
        if not os.path.exists(work_dir + "oss"):
            os.mkdir(work_dir + "oss")
    else:
        os.makedirs(work_dir + "oss", exist_ok=True)
        os.makedirs(overlay_dir, exist_ok=True)

    # look through places and find x and y values for certain orbital sites
    dem_site_num = orb_site_dict[dem_inp]
    places_curl_cmder(dem_site_num, site, drive, work_dir)
    tmp, y_dem, x_dem = pos_finder(dem_site_num, work_dir)
    #y_dem = 15331.9972100006
    #x_dem = -35736.98381
    print("x_dem: ", x_dem)
    print("y_dem: ", y_dem)
    dem_sl_f = -x_dem
    dem_ss_f = y_dem
    dem_sl = str(int(round(dem_sl_f)))
    dem_ss = str(int(round(dem_ss_f)))
    dem_sl = format(float(dem_sl_f), '.0f')
    dem_ss = format(float(dem_ss_f), '.0f')
    p = subprocess.Popen([
        os.environ['R2LIB'] + '/list',
        DEM,
        "nl=1", "ns=1", "sl=" + dem_sl, "ss=" + dem_ss, "ndigits=8"],
        stdin=PIPE, stdout=PIPE, stderr=PIPE)
    print(
        os.environ['R2LIB'] + '/list',
        DEM,
        "nl=1", "ns=1", "sl=" + dem_sl, "ss=" + dem_ss, "ndigits=8")
    output, err = p.communicate(
        b"input data that is passed to subprocess' stdin")
    rc = p.returncode
    output = output.decode("utf-8")
    print("output: ", output)
    print("rc: ", rc)
    o_list = output.split()
    dn = o_list[-1]
    dem_dn = format(float(dn), '.4f')
    print("dem_dn: ", dem_dn)
    dem_scale = '1'
    # dem_size should be a fourth of skin_size. in default this will be 257
    # the plus one will be subtracted in some parts.
    dem_size = str((int(skin_size) / 4) + 1)
    dem_size = dem_size.strip(".0")

    places_curl_cmder(0, site, drive, work_dir, site_flag=True)
    origin_offset_z, origin_offset_y, origin_offset_x = pos_finder(
        0, work_dir, site_flag=True)
    # Set just for testing
    #origin_offset_x = -94.63831
    #origin_offset_y = -40.478535
    #origin_offset_z = -11.229267

    print(
        "o_offset_x: ", origin_offset_x, " offset_y: ", origin_offset_y,
        " offset_z: ", origin_offset_z)

    demlist_sl, demlist_ss = sl_ss_math(
        x_dem, y_dem, dem_size, origin_offset_x, origin_offset_y,
        tilelist=tilelist, scale=1)

    print("demlist_sl: ", demlist_sl)
    print("demlist_ss: ", demlist_ss)
    dem_dn = float(dem_dn)
    dem_val = dem_dn + origin_offset_z

    R2LIB_copy(
        DEM, work_dir, "DEM", demlist_sl, demlist_ss,
        dem_size, dem_size, demlist_sl, demlist_ss)

    mesh_location_dict = {}

    # Start Looping here Aaron

    for skin in skin_list:
        # iterate through all the textures to apply to the mesh
        print("This is the skin type: ", skin)

        try:
            scale = float(scale_dict[skin])
            # skin_size = (skin_size * scale)  # this is wrong
            texture_path = texture_dict[skin]
            site_num = orb_site_dict[skin]

            places_curl_cmder(site_num, site, drive, work_dir)
            tmp, y_temp, x_temp = pos_finder(site_num, work_dir)

            # Set only for testing from here
            #if site_num == '5':
            #    y_temp = 17077.4900000002
            #    x_temp = -39354.059
            #elif site_num == '9':
            #    y_temp = 3325.49000000022
            #    x_temp = -10518.059
            #else:
            #    y_temp = 15331.9972100006
            #    x_temp = -35736.98381
            # to here
            print("x_temp: ", x_temp, " y_temp: ", y_temp)

            list_sl, list_ss = sl_ss_math(
                x_temp, y_temp, skin_size, origin_offset_x, origin_offset_y,
                tilelist=tilelist, scale=scale)

            R2LIB_copy(
                texture_path, work_dir, skin, list_sl, list_ss,
                skin_size, skin_size, list_sl, list_ss)

        except KeyError:
            print("Assuming a to scale texture is used")
            list_sl = demlist_sl
            list_ss = demlist_ss

        for ovr in ovr_list:
            ovr = ovr.upper()
            # iterate through list of specified overlays to create
            ovr_path = overlay_dict[ovr]
            R2LIB_copy(
                ovr_path, overlay_dir, ovr, list_sl, list_ss,
                skin_size, dem_size, demlist_sl, demlist_ss)

        if bigger:
            make_mesh_call(
                work_dir, skin, demlist_sl, demlist_ss, list_sl, list_ss,
                dem_size, skin_size, dem_scale, dem_val, "skin",
                tilelist=tilelist)
        else:
            make_mesh_call(
                work_dir, skin, demlist_sl, demlist_ss, list_sl, list_ss,
                dem_size, skin_size, dem_scale, dem_val, "skin")

        mesh_loc = merge_mesh_call(
            work_dir, skin, sol, site, drive, location_dir=output_dir)
        # Save the location of the output mesh in a dict to print later
        mesh_location_dict[skin] = mesh_loc

    if mesh_location_dict:
        for key, val in mesh_location_dict.items():
            print(key + ' mesh located: ' + val)


if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser(
        description="Orbital mesh maker." +
        "" +
        "",
        formatter_class=argparse.RawTextHelpFormatter)

    parser.add_argument('-ssd',
                        '--solsitedrive',
                        nargs=3,
                        metavar=('sol', 'site', 'drive'),
                        type=int,
                        help='Sol, site, drive in that order')
    # Texture skin size. needs to be 4 times dem size. it's fine as it is now
    parser.add_argument('-sz',
                        '--size',
                        type=int,
                        default=1024,
                        help="overlay skin size. STILL NEEDS WORK DONT USE")

    parser.add_argument('-r',
                        '--request',
                        default='ods',
                        help="Specify type of request.")

    parser.add_argument('-t',
                        '--texture',
                        nargs='+',
                        default=['BSM', 'BWP'],
                        help="Provide a list of textures to turn into meshes")

    parser.add_argument('-d',
                        '--dem',
                        default='DEM',
                        help="Provide a dem to put textures on")

    parser.add_argument('-ovr',
                        '--overlay',
                        nargs='+',
                        default=['ORR', 'SLP'],
                        help="Provide a list of overlays to turn into meshes")

    parser.add_argument('-l',
                        '--location',
                        nargs=3,
                        metavar=('x', 'y', 'z'),
                        type=float,
                        help="Location to spread tiles from. not implemented")

    parser.add_argument('-b',
                        '--bigger',
                        nargs=4,
                        metavar=('north', 'east', 'south', 'west'),
                        type=int,
                        help='Tile count to increase for each direction')

    parser.add_argument('-o',
                        '--outputdir',
                        type=str,
                        help='Specify output dir')

    args = parser.parse_args()
    print(args)
    main(args.solsitedrive, skin_size=args.size, req_type=args.request,
         bigger=args.bigger, ovr_list=args.overlay, dem_inp=args.dem,
         outputdir=args.outputdir, skin_list=args.texture, o_loc=args.location)
