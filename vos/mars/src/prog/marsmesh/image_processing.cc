//image_processing.cc
//Support functions for marsmesh program
//
#include "image_processing.h"

////////////////////////////////////////////////////////////////////////

// return false if point is invalid
bool getPoint3D(SimpleImage<float> *image, int line, int sample, double *xyz) {
    xyz[0] = image->get(0, line, sample);
    xyz[1] = image->get(1, line, sample);
    xyz[2] = image->get(2, line, sample);

    if (xyz[0] < -90000.0)    // invalid X
        return false;

     // check if the point is valid
    if (xyz[0] == 0.0 && xyz[1] == 0.0 && xyz[2] == 0.0)
        return false;

     return true;

}

// return false if passed point is invalid
bool setPoint3D(SimpleImage<float> *image, int line, int sample, double *xyz) {

     if (xyz == NULL)
         return false;

     image->set(0, line, sample, xyz[0]);
     image->set(1, line, sample, xyz[1]);
     image->set(2, line, sample, xyz[2]);

     return true;
}

// Resample one 1/2-size output range map cell at (i,j) from
// input map(i2,j2). No averaging, just subsample.
// dx/dy is direction to check for alternate points if specified cell
// is invalid.
void resample (SimpleImage<float> *iimg,SimpleImage<float> *oimg,
               int i, int j, int i2, int j2, int dx, int dy) {

    double ctr[3];
    if (!getPoint3D(iimg, j2, i2, ctr)) {  // center is invalid, try neighbors
        if (!getPoint3D(iimg, j2, i2+dx, ctr)) {
            if (!getPoint3D(iimg, j2+dy, i2, ctr)) {
                if (!getPoint3D(iimg, j2+dy, i2+dx, ctr)) {
                    oimg->set(0, j, i, -100000.0f);
                    oimg->set(1, j, i, -100000.0f);
                    oimg->set(2, j, i, -100000.0f);
                    return;
                }
            }
        }
    }
    oimg->set(0, j, i, ctr[0]);
    oimg->set(1, j, i, ctr[1]);
    oimg->set(2, j, i, ctr[2]);
    return;
}

// Resample one 1/2-size output range map cell at (i,j) from
// input map(i2,j2). No averaging, just subsample.
// dx/dy is direction to check for alternate points if specified cell
// is invalid.
void resampleX (SimpleImage<float> *iimg,SimpleImage<float> *oimg,
               int i, int j, int i2, int dx) {

    double ctr[3];
    if (!getPoint3D(iimg, j, i2, ctr)) {  // center is invalid, try neighbors
        if (!getPoint3D(iimg, j, i2+dx, ctr)) {
                    oimg->set(0, j, i, -100000.0f);
                    oimg->set(1, j, i, -100000.0f);
                    oimg->set(2, j, i, -100000.0f);
                    return;
        }
    }
    oimg->set(0, j, i, ctr[0]);
    oimg->set(1, j, i, ctr[1]);
    oimg->set(2, j, i, ctr[2]);
    return;
}


/// Resample xyz image to 1/2 resolution.
// Special ordering is to avoid shrinking meshes.
void image_resample(SimpleImage<float> *iimg, SimpleImage<float> *oimg) {
    int i, j;   //output (half-size) column/row
    int yres = iimg->getNL();
    int xres = iimg->getNS();

    int ictr = xres/4;  // output center column/row
    int jctr = yres/4;
    int imax = xres/2 - 1;  // output last column/row
    int jmax = yres/2 - 1;

    // loop on output entries, working from corners inward
    // (some output cells will be recomputed a few times...)
    int i2, j2;
    for (j=j2=0; j<jctr; j++, j2+=2) {
        for (i=i2=0; i<ictr; i++, i2+=2) {
            resample(iimg, oimg, i,      j,      i2,        j2, 1, 1);
            resample(iimg, oimg, imax-i, j,      xres-1-i2, j2, -1, 1);
            resample(iimg, oimg, i,      jmax-j, i2,        yres-1-j2, 1, -1);
            resample(iimg, oimg, imax-i, jmax-j, xres-1-i2, yres-1-j2, -1, -1);
         }
     }
     // fill in center row/column, one direction only
     i2 = 2*ictr;
     for (j=j2=0; j<=jmax; j++, j2+=2)
         resample(iimg, oimg, ictr, j, i2, j2, 1, 1);
         j2 = 2*jctr;
     for (i=i2=0; i<=imax; i++, i2+=2)
         resample(iimg, oimg, i, jctr, i2, j2, 1, 1);
}


/// Resample xyz image to 1/2 resolution. Only in X dimension
// Special ordering is to avoid shrinking meshes.
void image_resample_X(SimpleImage<float> *iimg, SimpleImage<float> *oimg) {
    int i, j;   //input (full-size) column/ full size row
    int yres = iimg->getNL();
    int xres = iimg->getNS();

    int yres_out = yres;
    int xres_out = (int) xres/2;
    double ctr[3];

    // loop on output entries
    for (j=0; j<yres; j++) {
        for (i=0; i<xres; i+=2) {
            ctr[0] = ctr[1] = ctr[2] = -100000.0f;
            if (!getPoint3D(iimg, j, i, ctr)) {  // center is invalid, try neighbors
                getPoint3D(iimg, j, i+1, ctr);
            }
            oimg->set(0, j, i/2, ctr[0]);
            oimg->set(1, j, i/2, ctr[1]);
            oimg->set(2, j, i/2, ctr[2]);
         }
     }
}

// Estimate surface normal for XYZ image at cell x, y by averaging
// normals of the 4 triangles using that cell (or fewer, at the edges
// and adjacent to invalid points).
void range_normal(SimpleImage<float> *image, int x, int y, int xres, int yres, double *norm)
{
        int valid = 0;                  // found a valid normal yet?

        if (x > 0 && y > 0)             // upper left triangle
            valid += add_one_normal(image, x, y, x-1, y, x, y-1, valid, norm);
        if (x+1 < xres && y > 0)        // upper right triangle
            valid += add_one_normal(image, x, y, x, y-1, x+1, y, valid, norm);
        if (x > 0 && y+1 < yres)        // lower left triangle
            valid += add_one_normal(image, x, y, x, y+1, x-1, y, valid, norm);
        if (x+1 < xres && y+1 < yres)   // lower right triangle
            valid += add_one_normal(image, x, y, x+1, y, x, y+1, valid, norm);

        if (valid) {            // normalize accumulated result
                normalize_vector(norm);
        } else {                // none valid, set default vertical normal
                norm[0] = norm[1] = 0.0;
                norm[2] = 1.0;
        }
}

// Compute normal for one triangle in XYZ image
// (2-1-3 = counterclockwise corner in left-handed image), accumulate
// for average cell normal. Return 1 if okay (all points are valid)
int add_one_normal(SimpleImage<float> *image, int x1, int y1, int x2,
                                int y2, int x3, int y3, int valid, double *norm)
{
        // Minimum cosine of angle between normal vectors. Smaller cosine
        // (larger angle) indicates a discontinuity.
        double min_cos_norm = 0.5;       // default = 60 degrees

        double p1[3], p2[3], p3[3], tnorm[3];

        if (!getPoint3D(image, y1, x1, p1) || !getPoint3D(image, y2, x2, p2)
            || !getPoint3D(image, y3, x3, p3))
                return 0;
        surface_normal(p1, p3, p2, tnorm);

        if (!valid) {           // first normal for this cell
                memcpy(norm, tnorm, sizeof(tnorm));

        // Accumulate, unless apparently a discontinuity.
        } else if (fabs(dot_product(tnorm, norm)) > min_cos_norm) {
                vector_sum(norm, tnorm, norm);
        }

        return 1;
}

bool  mask_image_by_tile_resolution(SimpleImage<float> *inp_image, 
                                    SimpleImage<int> *mask_image, 
                                    int res_level, 
                                    SimpleImage<float> *& out_image) {
    int i, j;   //input (full-size) column/ full size row
    int yres = inp_image->getNL();
    int xres = inp_image->getNS();
    //char msg[256];
    double xyz_pt[3];

    // loop on output entries
    for (j=0; j<yres; j++) {
        for (i=0; i<xres; i++) {
            if (mask_image->get(0,j,i) == res_level)
                getPoint3D(inp_image, j, i, xyz_pt);
            else {
                xyz_pt[0] = xyz_pt[1] = xyz_pt[2] == 0.0;
            }

            setPoint3D(out_image, j, i, xyz_pt);

            //snprintf(msg, 256, "dn: %lf, %lf, %lf\n", xyz_pt[0], xyz_pt[1], xyz_pt[2]);
            //snprintf(msg, 256, "res:  %ld\n", res_level);
            //zvmessage(msg, "");
                
         }
     }
    return false;
}

bool  copy_image(SimpleImage<float> *inp_image, SimpleImage<float> *& out_image) {
    int i, j;   //input (full-size) column/ full size row
    int yres = inp_image->getNL();
    int xres = inp_image->getNS();
    char msg[256];
    double xyz_pt[3];

    // loop on output entries
    for (j=0; j<yres; j++) {
        for (i=0; i<xres; i++) {
            getPoint3D(inp_image, j, i, xyz_pt);
            setPoint3D(out_image, j, i, xyz_pt);
         }
                
     }
    return true;
}

/// Accumulate interpolation at specified map cell, if valid.
// wnew = distance from hole being interpolated
// Return 1 if valid cell
int acc3(SimpleImage<float> *inimg, int x, int y, double wnew, double acc[], double *wsum)
{
    double r[3];
    //if (get_cell(x, y, r))
    if (!getPoint3D(inimg, y, x, r))
        return 0;
    acc[0] += wnew * r[0];
    acc[1] += wnew * r[1];
    acc[2] += wnew * r[2];
    *wsum += wnew;
    return 1;
}


// Weighted interpolation of nearest non-empty cells to
// fill in an empty cell. Updates imap and returns true if successful.
// !!!! maybe should check that points are nearly coplanar?
bool interpolate (SimpleImage<float> *iimg, SimpleImage<float> *tmpimg, int x, int y, int maxgap) {

    double acc[3];  // accumulators (X, Y, Z)
    double wsum;    // accumulated weight
    int nnen = 0;   // number of non-empty neighbors
    acc[0] = acc[1] = acc[2] = wsum = 0.0;

    int yres = iimg->getNL();
    int xres = iimg->getNS();


    for (int d=1; d<=maxgap; d++) {
        // accumulate non-empty cells in rectangle d units from x,y
        double wnew = 1.0 / d;
        int dx, dy;
        int x1 = x - d; if (x1 < 0) x1 = 0;
        int x2 = x + d; if (x2 >= xres) x2 = xres-1;

        dy = y - d;     // top row
        if (dy >= 0) {
            for (dx=x1; dx<=x2; dx++)
                nnen += acc3(iimg, dx, dy, wnew, acc, &wsum);
        }
        dy = y + d;     // bottom row
        if (dy < yres) {
            for (dx=x1; dx<=x2; dx++)
                nnen += acc3(iimg, dx, dy, wnew, acc, &wsum);
        }

        int y1 = y - d + 1; if (y1 < 0) y1 = 0;
        int y2 = y + d - 1; if (y2 >= yres) y2 = yres-1;

        dx = x - d;     // left column
        if (dx >= 0) {
            for (dy=y1; dy<=y2; dy++)
                nnen += acc3(iimg, dx, dy, wnew, acc, &wsum);
        }
            dx = x + d;     // right column
        if (dx < xres) {
            for (dy=y1; dy<=y2; dy++)
                nnen += acc3(iimg, dx, dy, wnew, acc, &wsum);
        }
        if (nnen >= 4) {    // successful, update temp map
            double xyz[3] = {acc[0]/wsum, acc[1]/wsum, acc[2]/wsum};
            setPoint3D(tmpimg, y, x, xyz);
            return true;
        }
    }

    return false;
}

// interpolate across gaps in range image
void image_range_interpolate(SimpleImage<float> *inimg, int maxgap) {

#define MSG_SIZE 256
    char msg[MSG_SIZE];
    int i, j;   // column/row
    double xyz[3];
    double r[3];
    int yres = inimg->getNL();
    int xres = inimg->getNS();

    // to avoid using interpolated gap cells in future interpolation,
    // put them into a temporary map
    SimpleImage<float> *tmpImg = new SimpleImage<float>(3, yres, xres);

    int num_int = 0;    // number of interpolated cells (diagnostic)
    for (j=0; j<yres; j++) {
        for (i=0; i<xres; i++) {
            xyz[0] = -100000.0;
            xyz[1] = 0.0;
            xyz[2] = 0.0;
            setPoint3D(tmpImg, j, i, xyz);
            //if (get_cell(i, j, r))
            if (!getPoint3D(inimg, j, i, r))
                num_int += interpolate(inimg, tmpImg, i, j, maxgap);
        }
    }

    bool verbose = true;
    verbose = false;
    if (verbose) {
        snprintf(msg, MSG_SIZE, "%d range map holes interpolated\n", num_int);
        zvmessage(msg, "");
    }

    // now safe to copy interpolated cells back to input map
    for (j=0; j<yres; j++) {
        for (i=0; i<xres; i++) {
            //r[0] = imap->get_float(i, j, 0);
            if (getPoint3D(tmpImg, j, i, r)) //valid
                setPoint3D(inimg, j, i, r);
        }
    }
}

SfcModel* init_octree(double max_x, double min_x, double max_y, double min_y, double max_z, double min_z, double max_edge, double min_edge, double zstretch) {
      // Choose the largest of the difference between the x, y, z to
        // determine the scale (largest dimension of the bounding box).
        double scale = max_x - min_x;
        if ( (max_y - min_y) > scale)
            scale = max_y - min_y;
        if ( (max_z - min_z) > scale)
            scale = max_z - min_z;

        // If had just one point, scale could be 0 because max_x - min_x = 0.
        if (scale < max_edge)
            scale = max_edge;

        // Calculate the octree depth
        // 2^levels = depth
        // levels = log (Base 2) depth = log (depth) / log (2);
        // round up
        // Depth is the ratio of the largest boundary dimension to the smallest
        // voxel
        double depth = scale/min_edge;

        // levels is the number of levels needed in the octree
        int levels = int(ceil( log(depth)/log(2.0) )) + 1;
        //if (verbose) {
         //   snprintf(msg, MSG_SIZE, "scale = %lf, min_edge = %lf, depth = %lf, depth = %lf, level = %d\n",
          //          scale, min_edge, scale/min_edge, depth, levels);
           // zvmessage(msg, "");
        //}

        // In the case of 1 point, we could end up with a levels of 0, which
        // is impossible.  Therefore, make sure levels is at least 1.
        if (levels <= 0)
            levels = 1;

        // Create the octree/surface model
        //return new SfcModel(levels);
        SfcModel* octree = new SfcModel(levels);
     //tm = &(octree->mesh);
     //tm->clean_up();
                // Setup model-object transformation matrix
        octree->x = -(max_x + min_x)/scale;
        octree->y = -(max_y + min_y)/scale;
        //!!!! ozp octree->z = -(max_z + min_z) * zstretch / scale;
        octree->z = 0.0;
        octree->xrot = octree->yrot = octree->zrot = 0.0;
        scale *= 0.5;           // modify scale since volume ranges -1 to 1
        octree->xscale = octree->yscale = scale;
        octree->zscale = scale / zstretch;
        return octree;
}


SfcModel* populate_octree(SimpleImage<float> *inimg, SfcModel *in_octree, long *num_polygons, double *range_out_min, double *range_out_max, bool create_and_init_octree_only, double c_pt[3], ZMatrix modelToWorld, double mdlcam[3], double pixfov, double max_angle, double range_low, double range_high) {


    Triangle_Model *tm;              // triangle mesh
    SfcModel* octree = in_octree;
    double zstretch = 1.0;
   
    int yres = inimg->getNL();
    int xres = inimg->getNS();  

    // allocate array to track previous row's nodes for mesh
    NodeSpec **prow = new NodeSpec *[xres];
    // Allocate space for edge storage
    int edge_size = xres*yres;

    char msg[MSG_SIZE];
    double *edge_matrix;
    edge_matrix = new double[edge_size];
    memset(edge_matrix, 0, edge_size * sizeof(double));
    // find data limits in image
    // Get the Z values to compute the depth, and then the level
    int edge_index;
    double  min_x, max_x;
    double  min_y, max_y;
    double  min_z, max_z;
    double  range_min, range_max;
    double  min_edge, max_edge;
    double scale;
    bool verbose = true;
    verbose = false;
    double imgxyz[3];

    min_x = min_y = min_z = range_min = min_edge = FLT_MAX;
    max_x = max_y = max_z = range_max = max_edge = -FLT_MAX;

    int invalid_counter = 0;
    int minrange_counter = 0;
    int maxrange_counter = 0; 

    PigPoint xyz;
    int number_of_valid_pts = 0;
    //!!! ozp adjust range_low, range_high to offsetted values
    for(int j=0; j<yres; j++) {  //line counter 
        edge_index = j * xres;  //column counter 
        for(int i=0; i<xres; i++, edge_index++) {
            PigPoint xyz_cm; 
            double cm_pt[3];
            if (!getPoint3D(inimg, j, i, imgxyz))
                continue;
            number_of_valid_pts++;
                      double tmp[3]; 
            sub3(c_pt, imgxyz, tmp);
            //apply offset of camera location to global/site coords  
            xyz_cm.setXYZ(tmp);

            // Set edge here
            // double dcam[3];
            double pt_range=xyz_cm.magnitude();
            xyz_cm.getXYZ(cm_pt);

            if (pt_range < range_low) {
                minrange_counter++;
                edge_matrix[edge_index] = -1.0;
                continue;
            }
 
            if (pt_range > range_high) {
                maxrange_counter++;
                edge_matrix[edge_index] = -1.0;
                continue;
            }
 
            //convert back to global/site
            xyz_cm.setXYZ(imgxyz);

           if(xyz_cm.getX() < min_x) min_x = xyz_cm.getX();
            if(xyz_cm.getX() > max_x) max_x = xyz_cm.getX();
            if(xyz_cm.getY() < min_y) min_y = xyz_cm.getY();
            if(xyz_cm.getY() > max_y) max_y = xyz_cm.getY();
            if(xyz_cm.getZ() < min_z) min_z = xyz_cm.getZ();
            if(xyz_cm.getZ() > max_z) max_z = xyz_cm.getZ();

            edge_matrix[edge_index] = pt_range*pixfov;

            // The smallest range will indicate the smallest edge.
            if (pt_range < range_min) {
                range_min = pt_range;
                min_edge = edge_matrix[edge_index];
            }

            if (pt_range > range_max) {
                range_max = pt_range;
                max_edge = edge_matrix[edge_index];
            }
        
        }   //column counter
    }   //line counter            


    //maxrange_counter = 0;
    snprintf(msg, MSG_SIZE, "number_of_valid_pts=%d\n", number_of_valid_pts);
    zvmessage(msg, "");

    double pt_min[3] = {min_x, min_y, min_z};
    double pt_max[3] = {max_x, max_y, max_z};


    if (verbose) {
        snprintf(msg, MSG_SIZE, "range limits: %g to %g", range_min, range_max);
        zvmessage(msg, "");
        snprintf(msg, MSG_SIZE, "edge range is %g to %g", min_edge, max_edge);
        zvmessage(msg, "");
        snprintf(msg, MSG_SIZE, "x range is %g to %g", pt_min[0], pt_max[0]);
        zvmessage(msg, "");
        snprintf(msg, MSG_SIZE, "y range is %g to %g", pt_min[1], pt_max[1]);
        zvmessage(msg, "");
        snprintf(msg, MSG_SIZE, "z range is %g to %g", pt_min[2], pt_max[2]);
        zvmessage(msg, "");
     }
     if (min_edge <= 0.0) {
         snprintf(msg, MSG_SIZE, "Min_edge must be greater than zero, is %f\n",
                 min_edge);
         zvmessage(msg, "");
         delete [] edge_matrix;
         delete [] prow;
         return NULL;
     }

   if (octree == NULL) {        // create a new octree

        // Choose the largest of the difference between the x, y, z to
        // determine the scale (largest dimension of the bounding box).
        scale = max_x - min_x;
        if ( (max_y - min_y) > scale)
            scale = max_y - min_y;
        if ( (max_z - min_z) > scale)
            scale = max_z - min_z;

        // If had just one point, scale could be 0 because max_x - min_x = 0.
        if (scale < max_edge)
            scale = max_edge;

        // Calculate the octree depth
        // 2^levels = depth
        // levels = log (Base 2) depth = log (depth) / log (2);
        // round up
        // Depth is the ratio of the largest boundary dimension to the smallest
        // voxel
        double depth = scale/min_edge;

        // levels is the number of levels needed in the octree
        int levels = int(ceil( log(depth)/log(2.0) )) + 1;
        if (verbose) {
            snprintf(msg, MSG_SIZE, "scale = %lf, min_edge = %lf, depth = %lf, depth = %lf, level = %d\n",
                    scale, min_edge, scale/min_edge, depth, levels);
            zvmessage(msg, "");
        }

        // In the case of 1 point, we could end up with a levels of 0, which
        // is impossible.  Therefore, make sure levels is at least 1.
        if (levels <= 0)
            levels = 1;

        // Create the octree/surface model
        octree = new SfcModel(levels);
     tm = &(octree->mesh);
     tm->clean_up();
     } // end of create a new octree

     NodeSpec *ns = NULL, *pns = NULL;
     // double  cntr[OCTREE_DIMS];
     uchar red, green, blue;
     red=255;
     green=blue=0;

     // No more changes to transform parms, so precompute matrices
     // to speed up add_voxel()
     octree->freeze_xform();
     ZMatrix xform;
     octree->GetModelToObjectTransform(xform);

     //get voxel resolution in model space, convert to object space
     double mod_res = 4.0 / (1 << octree->get_max_levels());
     mod_res *= ((max_x - min_x) +
                 (max_y - min_y)+
                 (max_z - min_z))
                 / 3.0;

     if (verbose) {
       snprintf(msg, MSG_SIZE, "Voxel Resolution: %f, %f\n", mod_res, scale);
       zvmessage(msg, "");
       //printf("mod_res = %f, %f\n", mod_res, octree->get_max_levels());
     }

     // double gc[3], cc[3];
     MatPreMult(modelToWorld, xform);

     // map camera position from object coords to model space
     octree->GetObjectToModelTransform(xform);
     ZMatrix xxform;
     octree->GetModelToObjectTransform(xxform);
     MultPoints(c_pt, xform, mdlcam);

    // Only create and initialize octree
     // while processing the whole image
     if (create_and_init_octree_only)
         return octree;

     // Read through the range image and create voxels
     // Loop:  Read the range file one pixel at a time
     // for each pixel
     //    create a voxel
     //    get color and calculate edge

     int counter = 0;

     // Some xyz's will be "bad" and need to be tossed. kfs
     for(int j=0; j<yres; j++) {
        edge_index = j * xres;
        if (xres>1) {
            prow[xres-2] = pns;
            prow[xres-1] = ns;
        }
        ns = NULL;

        for(int i=0; i<xres; i++, edge_index++) {
            if (i>1)
                prow[i-2] = pns;
            pns = ns;

            // Skip invalid points
            if (edge_matrix[edge_index] <= 0.0) {
                ns = NULL;
            } else {
                ns = new NodeSpec();

                // Get color from rgb file, or default
                //if (rgb)
                //        rgb->get_color(i, j,
                //                        &red, &green, &blue);
                ns->set_color(red, green, blue);
                ns->use_alpha(TRUE);

                // Division is for scaling.
                ns->edge_length = edge_matrix[edge_index]/scale;

                //getxyz(i, j, cntr);
                getPoint3D(inimg, j, i, imgxyz);
                ns->set_global_center(imgxyz);

                // compute voxel surface normal vector
                double norm[3];

                octree->add_voxel(ns);
                counter++;
            }

            // update mesh, if not left column or top row
            if (/*(flags & R2O_MESH) &&*/ i>0 && j>0)
                add_mesh(prow[i-1], prow[i], pns, ns, tm, max_angle, mdlcam);

            // ** Add empties here
            // Empties will go from point to eye, getting smaller
            // as they get closer to the eye.
        }
    }  // tossing bad xyz

    // clean up
    delete [] edge_matrix;
    delete [] prow;

   if (verbose) {
        snprintf(msg, MSG_SIZE, "Found %d invalid points\n", invalid_counter);
        zvmessage(msg, "");
        snprintf(msg, MSG_SIZE, "Found %d <%f(min-range), %d >%f(max-range) points\n",
        minrange_counter, range_min, maxrange_counter, range_max);
        zvmessage(msg, "");
        //fprintf(stderr, "Discarded %d sliver triangles\n",
        //        sliver_count);
        //fprintf(stderr, "Rejected %d triangles for bad direction\n",
        //       angle_count);
        snprintf(msg, MSG_SIZE, "Added total of %d points\n", counter);
        snprintf(msg, MSG_SIZE,"Full-res mesh is %d triangles\n",octree->mesh.tri_count());
        zvmessage(msg, "");
    } //end of verbose

    *num_polygons = octree->mesh.tri_count();
    *range_out_min = range_min;
    *range_out_max = range_max;
    tm->clean_up();
    return octree;
}
