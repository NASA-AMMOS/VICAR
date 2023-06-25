#! /usr/bin/perl
#
##
#  This program evaluates the Vicar build logs for 
#  errors.
#
#  Input vicar build log name
#
$log = @ARGV[0];
$flag = @ARGV[1];
#
# Open log file
# 
open(LOG,$log) || die "The vicar build log file could not be opened.\n";
if ($flag eq "")
{  print "************Platform designation missing for build log.***********\n";
   print "Please enter linux32,linux64,linux32nsyt,linux32msl,linux64nsyt,linux64msl.\n"
}
$name = " ";
$errmsg = " ";
#
#######################################################################################
#
# Linux 32-bit Build Exception Reasons:
#
#
# 1) NO XRT LIB: ccdnoise,ccdrecip,ccdslope,mosplot,otf1,plot3d,plotint, pltgraf, power,
#             qplot2, statplt,tieplot and xrtps require the XRT library which is not available
#             on the sgi or linux.
# 2) NO PVM3 LIB: gllcntrl, gll_brws_dsp, isslab, mpfcntrl,mpfcordsquat, mpfimbuildimp,
#                 mpfimbuildapx, mpfimbuildrvr, mpfpredict, mpfrvrclr, mpfrvrpredict,
#                 mpftelemproc,rtdsplot,rtlogger,m98_ssi_pktmaker,m98predict 
#
# 3) NO SIMBAD LIB: simbad, simbadcat, sky, skycat, 
#       
# 4) SPURIOUS ERRORS: pgm_icons,rts_tcl_scripts,tp_pdf, xvd_pdf,rtcsetupvms
#    actually have no build.  The files only need to get unpacked.
#    The errors generated are spurious.
#
# 5) SOLR ONLY: ds1comm,ds1sfdu,ds1telemproc,ds1fileobj,ds1sirtfbase,isis2vic
#               casisstlmproc,casvimstlmproc,sirtftlmproc,cascommonsource,
#               casisslosslessdecompsource,casisssource,casvimssource,cas_tlm_cat,
#               cas_lplist,cas_pixlist,cas_ul_cck_subs_1,cas_ul_cck_subs_2,
#               cas_ul_globals_1,cas_ul_globals_2,
#               ul_cck_tool,
#               sirtfcomm, sirtfdatamodel, sirtffileobj, sirtftlmproc, suffix2core,
#               rovernav (Per Jean Lorre 10/26/00 will deliver for linux
#                         at later date.) 
#               ics2sasf, vioi2ics added 02/12/01
#               sclk2scet added 03/05/01, solr is the only platform noted on pkg form 
#               sirtf_rt,sirtf_h,sirtflib added 6/7/01: solr only per cxr's pkg form.
#               casworker_h,casworkersource add 6/7/01: solr only per rrp's pkge form.
#               tlm/prog/tlm_Cassini_ISS_stream,tlm_Cassini_VIMS_steam: solr only per pkg
#                                                                    tlm_collocated_streams-mpb
#               
#               cas_common_cat cas_swat_cat casswatsource castpspluginsource,
#               casissstdtlmproc casswat casvimsstdtlmproc added 11/27/01: 
#               solr only per ays.
#               tlm_SIRTF_SIRTF_stream added 11/27/01: solr only per mpb
#               tlm/prog/mertelemlib & mertelemproc added 9/6/02 per 
#               MerGds6.5 pkg pxz,cxr
#               cdrsgrg added 4/9/03 per rrp
#               casstdpluginsource added 8/8/03 per rrp
#               p2/prog/ads_server.com added 9/29/2004 per sxc
#
# 6) SOLR/ALPHA ONLY: vimslab (per Alice Stanboli, 10/2/00)
#               casvimscruisesource and casvimscruisestdtlmproc added 11/13/02
#               per sxc.
#
# 7) All other modules listed have not been ported to linux.
# 8) p2/prog/featherv.com delivered for alpha only by BAM, 1/3/01.
# 9) now supporting geoma per Barbara and RGD 6/18/02.
# 10) Modules now supported on LINUX: p2/prog/ibisupdate, overlay, sargonb  
#     Delivered by Gary Yagi in August 2002 for D29.0.1. 
# 11) Removed gll/prog/galsos.com from exception list per CDE Amy Chen, 1/10/03
# 12) Removed mars/src/sub/mertelemlib and mertelemproc per pkg MER_Upd_050603_GDS8.5
#     by Hyun Lee. 
# 13) XRT Lib: Removed ccdnoise,ccdrecip,ccdslope,mosplot,otf1,plot3d,plotint, pltgraf, power,
#             qplot2, statplt,tieplot and xrtps from the build exception list.
#             The XRT Library is now available on the LINUX platform (mipldevlinux4).
#             1/28/05 sxc 
# 14) P3 Routines: These routines were delivered by a summer hire for Barbara McGuffie. They do not build on linux.
#	           p3/sub/get_sebass_wavlen,timssubs,uniflt,zlow7, p3/prog/alphar,amer,append,astergeo,astertir,avhrrlog,
#                  bmss,boxflt2,c130rect,c,convim,demloga,destretch,eigen,envi2vic,fastclass,filter,fit,flot,fromascii,
#                  gainoff,garslog,genthis,gradrem,hist,insect,l7cal,lave,lookup,maskv,masterhot,mastertir,mastertoa,
#                  mastir,median,minmap,mivistir,mss,navlog,pars,pixgrad,pixstat,qsar,ratio0,reallist,repair,sargonb,
#                  sebasscal,sebasste,shady2,shady,simplify,size,specfil,spectrum,stats,stretch3d,stretch,stretchf,strippit,
#                  swapper,tcal2,temis,textad,tgeom2,tgeominv,timscal2,timscal2old,timscal2re,timscal,timsconv,
#                  timsemis,timslog,timsnav,timsresp,tmscal,tmscalit,to3d,tran,transect,trigrid,vic2srf,watermap,xform
#                  zfill.
#                   9/9/05
#
# 15) Removed from exception list because modules now build on linux:
#                 automatch,cas_lplist,colort,data_transfer,getpc,ibisgcp,insert3d,isslab,jpegcomp,manmatch,
#                 mpfpredict,mpfrvrclr,mpfrvrpredict,maplabprog,marsjplstereo,nav2,nimscmm2,nutinp
#                  mpf_compression_labels,mpf_rcw_daemon,mpfcordsquat,obs_cat,
#                  obs_list,phot_func,phot_list,ptp,ressar77,rice_comp,ricecomp,rice_decomp,
#                 rtdsplog,rtlogger,rts_tcl_scripts,sclk2scet,scet2sclk,simbad,simbadcat,sky,skycat,suffix2core
#                 time_value,vioi2ics,rtcsetupvms    3/8/06
# 16) Removed from exception list because modules have been obsoleted:
#                 cas_pixlist,casworkersource,cas_ul_cck_subs_1,cas_ul_cck_subs_2,cas_ul_globals_1,cas_ul_globals_2,
#                 catsup, craf, ds1*, gedread, m98*, nimscmm, nimsfloat,nimsobs,nimstsi2,
#                 pgm_icons,pwssnip,sirtf_rt, sirtfcomm,sirtfdatamodel,sirtf_h,sirtffileobj
#                 sirtftelemproc,sirtflib,sirtftlmproc,ssisnip,
#                 tlm_SIRTF_SIRTF_stream,ul_cck_tool,vimslab   3/8/06
#
# 17) P3 Routine:  temis 3/8/06  Part of summer hire delivery
#
# 18) tlm_base, tlm_event, tlm_Test_Test_stream: No longer build with RHE4, appears not to be needed by SIRTF. (3/22/07)
# 19) Removed tlm_event from exception list.  Module builds with no errors.  (6/19/2009) 
# 20) Added galsos.  (1/28/2010) 
# 21) Removed the following modules from list because they now build on Linux platform:
#     get_sebass_wavlen,timssubs,uniflt,zlow7,alphar,amer,append,astergeo,astertir,avhrrlog, 
#     bmss,boxflt2,c130rect,c,convim,demloga,destretch,eigen,envi2vic,fastclas,filter,fit,flot,fromascii,
#     gainoff,garslog,genthis,gradrem,hist,insect,l7cal,lave,lookup,
#     maskv,masterhot,mastertir,mastertoa,mastir,median,minmap,mivistir,mss,navlog  
#     pixgrad,pixstat,qsar,ratio0,reallist,repair,sargonb,sebasscal,sebasste,shady,simplify,size,
#     specfil,spectrum,stats,stretch3d,stretch,stretchf,strippit,swapper,textad,tgeom2, 
#     tgeominv,timscal2,timscal2old, timscal2re, timscal,timsconv,
#     timsemis,timslog,timsnav,timsresp,tmscal,tmscalit,to3d, tran,transect,trigrid    
#     watermap,xform,zfill,arc2graf,cascommonsource,casvimssource,casisslosslessdecompsource,casisssource,
#     casswat,casstdpluginsource,casswatsource,casvimsstdtlmproc,cas_common_cat,cas_swat_cat,cas_tlm_cat,cdrsgrg,clusan,
#     clustest,featherv,fcnpolar,graf2arc,ibisnav,ics2sasf2,interloc,jpegfix, 
#     oldgeoma2ibis,oospice_sub,paint,polypmap,vtiff,tlm_base
# 22) Removed the following modules becasue they no longer exist on the system:
#     pars,ads_server,castpspluginsource,casvimscruisesource,casvimscruisestdtlmproc,casvimstlmproc,
#     casisstlmproc,isis2vic,mpf_brws_dsp,mpf_rts_dsp,mpf_spice_time,mpfcatlbl,mpfcntrl,
#     mpfimbuildapx,mpfimbuildimp,mpfimbuildrvr,mpftelemproc,reseaulog, 
#     tlm_Cassini_ISS_stream,tlm_Cassini_VIMS_stream,xvd_pdf,tp_pdf 
#
# 23) Added the XRT routines: xrtps,ccdnoise,ccdrecip,ccdslope,mosplot,otf1,plot3d,
#     plotint, pltgraf, power,qplot2, statplt and tieplot. No XRT license available.  5/8/13
# 24) Removed from exception list:galsos,ccdnoise,ccdrecip,ccdslope,mosplot,otf1,plot3d,plotint,pltgraf,qplot2,tieplot
#     1/28/15
# 25) Added marsinterp,nsyt_instruments,nsytrough,nsyttilt. 9/17/15
# 26) Removed xrpts because it's been obsoleted.  11/17/15
# 27) Removed marsinterp,nsyt_instruments,nsytrough,nsyttilt, plotit, rovernav, xyznet because they now build on linux.  11/17/15
# 28) Added cat_gen_util, rts_cat_util, cas_cat, cas_sybase_util,mpl_cat_util after mdms code obsoleted.
# 29) Also added v2param,isslab,suffix2core,vimslab. 9/24/18
# 30) Added marsrefmesh per Francois Ayoub. 1/30/19
# 31) Added mars python scripts, m2020_scam_fitsgen, m2020_update_scam_odl 11/13/19
# 32) Removed meshman, marsecorr due to tps embree not available for Linux-32. 2/20/20
# 33) Removed m2020_scam_gitsgen,m2020_updat_scam_odl.  1/6/21
# 34) Removed cat_gen_util,rts_cat_util,cas_cat,cas_sybase_util,mpl_cat_util,isslab,
#     vimslab as they have been obsolete due to Sybase being no longer used. (1/11/21)
# 35) Removed suffix2core. Suffix2core has been modified to not use Sybase. (1/11/21)
# 36) Added tcl2tcl. p2/prog/tcl2tcl builds only on 64-bit. (5/24/21)
# 37) Added p3/ztimsfilt. Actually,ztimsfilt builds on 32-bit however crumbs does not and
#     because of the way crumbs builds,it shows the previous item that was built
#     as the build error. (2/15/22)
# 38) Added mars/src/prog/posegraph.Build only on 64-bit RH8 and 64 & 32-bit RH7. (10/3/22)
# 39) Added these AFIDS routines recently added to Vicar. At this time they do not build on either
#     RH8 32 or 64-bit:acca,alilog,fytpe2tcl,hyperionlog,ibislsq9,modislog_mod021km,paintc,roc_fit,
#     shp2vic,vic2shp,vtiff3 (10/4/22) per Bob Crocco.
# 40) Added marsnav2. TPS not rebuilt for 32-bit RH8. 10/5/22
#
########################################################################################

@exceptlinux32=("marsrefmesh","meshman","marsecorr","tcal2","temis","vic2srf","v2param","tcl2tcl","ztimsfilt",
"posegraph","acca","alilog","ftype2tcl","hyperionlog","ibislsq9","modislog_mod021km","paintc",
"rpc_fit","shp2vic","vic2shp","vtiff3","marsnav2");

#
#######################################################################################
#
# Linux 64-bit Build Exception Reasons:
#
#
# 1) NO XRT LIB: ccdnoise,ccdrecip,ccdslope,mosplot,otf1,plot3d,plotint, pltgraf, power,
#             qplot2, statplt,tieplot and xrtps require the XRT library which is not available
#             on the sgi or linux.
# 2) NO PVM3 LIB: gllcntrl, gll_brws_dsp, isslab, mpfcntrl,mpfcordsquat, mpfimbuildimp,
#                 mpfimbuildapx, mpfimbuildrvr, mpfpredict, mpfrvrclr, mpfrvrpredict,
#                 mpftelemproc,rtdsplot,rtlogger,m98_ssi_pktmaker,m98predict 
#
# 3) NO SIMBAD LIB: simbad, simbadcat, sky, skycat, 
#       
# 4) SPURIOUS ERRORS: pgm_icons,rts_tcl_scripts,tp_pdf, xvd_pdf,rtcsetupvms
#    actually have no build.  The files only need to get unpacked.
#    The errors generated are spurious.
#
# 5) SOLR ONLY: ds1comm,ds1sfdu,ds1telemproc,ds1fileobj,ds1sirtfbase,isis2vic
#               casisstlmproc,casvimstlmproc,sirtftlmproc,cascommonsource,
#               casisslosslessdecompsource,casisssource,casvimssource,cas_tlm_cat,
#               cas_lplist,cas_pixlist,cas_ul_cck_subs_1,cas_ul_cck_subs_2,
#               cas_ul_globals_1,cas_ul_globals_2,
#               ul_cck_tool,
#               sirtfcomm, sirtfdatamodel, sirtffileobj, sirtftlmproc, suffix2core,
#               rovernav (Per Jean Lorre 10/26/00 will deliver for linux
#                         at later date.) 
#               ics2sasf, vioi2ics added 02/12/01
#               sclk2scet added 03/05/01, solr is the only platform noted on pkg form 
#               sirtf_rt,sirtf_h,sirtflib added 6/7/01: solr only per cxr's pkg form.
#               casworker_h,casworkersource add 6/7/01: solr only per rrp's pkge form.
#               tlm/prog/tlm_Cassini_ISS_stream,tlm_Cassini_VIMS_steam: solr only per pkg
#                                                                    tlm_collocated_streams-mpb
#               
#               cas_common_cat cas_swat_cat casswatsource castpspluginsource,
#               casissstdtlmproc casswat casvimsstdtlmproc added 11/27/01: 
#               solr only per ays.
#               tlm_SIRTF_SIRTF_stream added 11/27/01: solr only per mpb
#               tlm/prog/mertelemlib & mertelemproc added 9/6/02 per 
#               MerGds6.5 pkg pxz,cxr
#               cdrsgrg added 4/9/03 per rrp
#               casstdpluginsource added 8/8/03 per rrp
#               p2/prog/ads_server.com added 9/29/2004 per sxc
#
# 6) SOLR/ALPHA ONLY: vimslab (per Alice Stanboli, 10/2/00)
#               casvimscruisesource and casvimscruisestdtlmproc added 11/13/02
#               per sxc.
#
# 7) All other modules listed have not been ported to linux.
# 8) p2/prog/featherv.com delivered for alpha only by BAM, 1/3/01.
# 9) now supporting geoma per Barbara and RGD 6/18/02.
# 10) Modules now supported on LINUX: p2/prog/ibisupdate, overlay, sargonb  
#     Delivered by Gary Yagi in August 2002 for D29.0.1. 
# 11) Removed gll/prog/galsos.com from exception list per CDE Amy Chen, 1/10/03
# 12) Removed mars/src/sub/mertelemlib and mertelemproc per pkg MER_Upd_050603_GDS8.5
#     by Hyun Lee. 
# 13) XRT Lib: Removed ccdnoise,ccdrecip,ccdslope,mosplot,otf1,plot3d,plotint, pltgraf, power,
#             qplot2, statplt,tieplot and xrtps from the build exception list.
#             The XRT Library is now available on the LINUX platform (mipldevlinux4).
#             1/28/05 sxc 
# 14) P3 Routines: These routines were delivered by a summer hire for Barbara McGuffie. They do not build on linux.
#                  p3/sub/get_sebass_wavlen,timssubs,uniflt,zlow7, p3/prog/alphar,amer,append,astergeo,astertir,avhrrlog,
#                  bmss,boxflt2,c130rect,c,convim,demloga,destretch,eigen,envi2vic,fastclass,filter,fit,flot,fromascii,
#                  gainoff,garslog,genthis,gradrem,hist,insect,l7cal,lave,lookup,maskv,masterhot,mastertir,mastertoa,
#                  mastir,median,minmap,mivistir,mss,navlog,pars,pixgrad,pixstat,qsar,ratio0,reallist,repair,sargonb,
#                  sebasscal,sebasste,shady2,shady,simplify,size,specfil,spectrum,stats,stretch3d,stretch,stretchf,strippit,
#                  swapper,tcal2,temis,textad,tgeom2,tgeominv,timscal2,timscal2old,timscal2re,timscal,timsconv,
#                  timsemis,timslog,timsnav,timsresp,tmscal,tmscalit,to3d,tran,transect,trigrid,vic2srf,watermap,xform
#                  zfill.
#                   9/9/05
#
# 15) Removed from exception list because modules now build on linux:
#                 automatch,cas_lplist,colort,data_transfer,getpc,ibisgcp,insert3d,isslab,jpegcomp,manmatch,
#                 mpfpredict,mpfrvrclr,mpfrvrpredict,maplabprog,marsjplstereo,nav2,nimscmm2,nutinp
#                  mpf_compression_labels,mpf_rcw_daemon,mpfcordsquat,obs_cat,
#                  obs_list,phot_func,phot_list,ptp,ressar77,rice_comp,ricecomp,rice_decomp,
#                 rtdsplog,rtlogger,rts_tcl_scripts,sclk2scet,scet2sclk,simbad,simbadcat,sky,skycat,suffix2core
#                 time_value,vioi2ics,rtcsetupvms    3/8/06
# 16) Removed from exception list because modules have been obsoleted:
#                 cas_pixlist,casworkersource,cas_ul_cck_subs_1,cas_ul_cck_subs_2,cas_ul_globals_1,cas_ul_globals_2,
#                 catsup, craf, ds1*, gedread, m98*, nimscmm, nimsfloat,nimsobs,nimstsi2,
#                 pgm_icons,pwssnip,sirtf_rt, sirtfcomm,sirtfdatamodel,sirtf_h,sirtffileobj
#                 sirtftelemproc,sirtflib,sirtftlmproc,ssisnip,
#                 tlm_SIRTF_SIRTF_stream,ul_cck_tool,vimslab   3/8/06
#
# 17) P3 Routine:  temis 3/8/06  Part of summer hire delivery
#
# 18) tlm_base, tlm_event, tlm_Test_Test_stream: No longer build with RHE4, appears not to be needed by SIRTF. (3/22/07)
# 19) Removed tlm_event from exception list.  Module builds with no errors.  (6/19/2009) 
# 20) Added galsos.  (1/28/2010) 
# 21) Removed the following modules from list because they now build on Linux platform:
#     get_sebass_wavlen,timssubs,uniflt,zlow7,alphar,amer,append,astergeo,astertir,avhrrlog, 
#     bmss,boxflt2,c130rect,c,convim,demloga,destretch,eigen,envi2vic,fastclas,filter,fit,flot,fromascii,
#     gainoff,garslog,genthis,gradrem,hist,insect,l7cal,lave,lookup,
#     maskv,masterhot,mastertir,mastertoa,mastir,median,minmap,mivistir,mss,navlog  
#     pixgrad,pixstat,qsar,ratio0,reallist,repair,sargonb,sebasscal,sebasste,shady,simplify,size,
#     specfil,spectrum,stats,stretch3d,stretch,stretchf,strippit,swapper,textad,tgeom2, 
#     tgeominv,timscal2,timscal2old, timscal2re, timscal,timsconv,
#     timsemis,timslog,timsnav,timsresp,tmscal,tmscalit,to3d, tran,transect,trigrid    
#     watermap,xform,zfill,arc2graf,cascommonsource,casvimssource,casisslosslessdecompsource,casisssource,
#     casswat,casstdpluginsource,casswatsource,casvimsstdtlmproc,cas_common_cat,cas_swat_cat,cas_tlm_cat,cdrsgrg,clusan,
#     clustest,featherv,fcnpolar,graf2arc,ibisnav,ics2sasf2,interloc,jpegfix, 
#     oldgeoma2ibis,oospice_sub,paint,polypmap,vtiff,tlm_base
# 22) Removed the following modules because they no longer exist on the system:
#     pars,ads_server,castpspluginsource,casvimscruisesource,casvimscruisestdtlmproc,casvimstlmproc,
#     casisstlmproc,isis2vic,mpf_brws_dsp,mpf_rts_dsp,mpf_spice_time,mpfcatlbl,mpfcntrl,
#     mpfimbuildapx,mpfimbuildimp,mpfimbuildrvr,mpftelemproc,reseaulog, 
#     tlm_Cassini_ISS_stream,tlm_Cassini_VIMS_stream,xvd_pdf,tp_pdf 
# 23) Added mslfilter.  It only builds on Linux 32-bit.  
#
# 24) Removed from exception list:galsos,ccdnoise,ccdrecip,ccdslope,mosplot,otf1,plot3d,plotint,pltgraf,qplot2,tieplot
#     statplt, plotit. 1/28/15
#
# 25) Added marsinterp.  9/17/15
# 26) Removed rovernav,xyznet because they now build on linux64. 11/17/15
# 27) Added cat_gen_util, rts_cat_util, cas_cat, cas_sybase_util,mpl_cat_util after mdms code obsoleted. 9/24/18
# 28) Added mars python scripts, m2020_scam_fitsgen, m2020_update_scam_odl 11/13/19
# 29) Added mars/src/prog/m20filter 2/12/20
# 30) Removed m2020_scam_fitsgen,m2020_update_scam_odl. 1/7/21
# 31) Removed cat_gen_util,rts_cat_util,cas_cat,cas_sybase_util,mpl_cat_util,isslab,
#     vimslab as they have been obsolete due to Sybase being no longer used. (1/11/21)
# 32) Removed suffix2core. Suffix2core has been modified to not use Sybase. (1/11/21) 
# 33) Added these AFIDS routines recently added to Vicar. At this time they do not build on either
#     RH8 32 or 64-bit:acca,alilog,fytpe2tcl,hyperionlog,ibislsq9,modislog_mod021km,paintc,roc_fit,
#     shp2vic,vic2shp,vtiff3 (10/4/22) per Bob Crocco.
#
#######################################################################################
#
@exceptlinux64=("tcal2","temis","vic2srf","mslreach","libpig_native","libdivl1b_a",
"libdivl1b_so","divl1b","mslfilter","m20filter",
"posegraph","acca","alilog","ftype2tcl","hyperionlog","ibislsq9","modislog_mod021km","paintc",
"rpc_fit","shp2vic","vic2shp","vtiff3");
#
#######################################################################################
#
# MSL Linux-32 Build Exception Reasons:
#
# Added m2020 and nsyt code. 6/12/20
# Removed m2020_scam_gitsgen,m2020_updat_scam_odl. 1/7/21
# Added marsnav2. 10/5/22
#
#######################################################################################
@exceptlinux32msl=("marsrefmesh","meshman","marsecorr","tcal2","temis","vic2srf",
"cat_gen_util","rts_cat_util","cas_cat","cas_sybase_util","mpl_cat_util",
"v2param","isslab","suffix2core","vimslab","m2020edrgenlib",
"m2020engcamedrgenlib","m2020edrgen","m2020moxieedrgenlib","m2020pixledrgenlib","m2020rimfaxedrgenlib",
"m2020srlcedrgenlib","m2020supercamedrgenlib","m20filter","nsyt_ida_science_telemproc","nsyt_ida_telemproc",
"m2020medaedrgenlib","m2020mmmcamedrgenlib","nsyt_twins_telemproc","phxreach","phxtelemproc",
"posegraph","acca","alilog","ftype2tcl","hyperionlog","ibislsq9","modislog_mod021km","paintc",
"rpc_fit","shp2vic","vic2shp","vtiff3","marsnav2");
#
#######################################################################################
#
#
# MSL Linux 64-bit Build Exception Reasons:
#
# Added m2020 and nsyt code. 6/12/20
#
################################################################################
@exceptlinux64msl=("tcal2","temis","vic2srf","mslreach","libpig_native","libdivl1b_a",
"libdivl1b_so","divl1b",
"mslfilter",
"m2020_scam_fitsgen","m2020_update_scam_odl","m20filter","m2020edrgenlib","m2020engcamedrgenlib",
"m2020medaedrgenlib","m2020mmmcamedrgenlib","m2020moxieedrgenlib","m2020pixledrgenlib",
"m2020rimfaxedrgenlib","m2020srlcedrgenlib","m2020supercamedrgenlib","m2020edrgen",
"posegraph","acca","alilog","ftype2tcl","hyperionlog","ibislsq9","modislog_mod021km","paintc",
"rpc_fit","shp2vic","vic2shp","vtiff3");
#
################################################################################
#
# NSYT Linux-32 Build Exception Reasons:
#
# Added m2020 code. 6/12/20
# Added marsnav2. 10/5/22
#
################################################################################
@exceptlinux32nsyt=("marsrefmesh","meshman","marsecorr","tcal2","temis","vic2srf",
"v2param","m2020_scam_fitsgen","m2020_update_scam_odl","m2020edrgenlib",
"m2020engcamedrgenlib","m2020edrgen","m2020moxieedrgenlib","m2020pixledrgenlib","m2020rimfaxedrgenlib",
"m2020srlcedrgenlib","m2020supercamedrgenlib","m20filter",
"m2020medaedrgenlib","m2020mmmcamedrgenlib","mslfilter","mslreach",
"posegraph","acca","alilog","ftype2tcl","hyperionlog","ibislsq9","modislog_mod021km","paintc",
"rpc_fit","shp2vic","vic2shp","vtiff3","marsnav2");
#
################################################################################
#
@exceptlinux64nsyt=("tcal2","temis","vic2srf","mslreach","libpig_native","libdivl1b_a",
"libdivl1b_so","divl1b",
"mslfilter",
"m2020_scam_fitsgen","m2020_update_scam_odl","m20filter","m2020edrgenlib","m2020engcamedrgenlib",
"m2020medaedrgenlib","m2020mmmcamedrgenlib","m2020moxieedrgenlib","m2020pixledrgenlib",
"m2020rimfaxedrgenlib","m2020srlcedrgenlib","m2020supercamedrgenlib","m2020edrgen",
"posegraph","acca","alilog","ftype2tcl","hyperionlog","ibislsq9","modislog_mod021km","paintc",
"rpc_fit","shp2vic","vic2shp","vtiff3");
#
###############################################################################


ERROR: 

while (<LOG>) {
       chomp;
       if (/^\#\s*module\s*(\w+)\s*$/) {
         $name = $1;
#       print "* module $name\n";
 
}


if ($flag eq "linux32") {
 if (/Error 1/||/No rule/) {
         $errmsg = $_;
         foreach $pattern (@exceptlinux32) {
           if ($name =~ $pattern) {
           next ERROR;}
        }
      print "\n";
      print "* module $name\n";
      print "$errmsg\n";
    }
}


elsif ($flag eq "linux64") {
 if (/Error 1/||/No rule/) {
         $errmsg = $_;
         foreach $pattern (@exceptlinux64) {
           if ($name =~ $pattern) {
           next ERROR;}
        }
      print "\n";
      print "* module $name\n";
      print "$errmsg\n";
    } 
}

elsif ($flag eq "linux64msl") {
 if (/Error 1/||/No rule/) {
         $errmsg = $_;
         foreach $pattern (@exceptlinux64msl) {
           if ($name =~ $pattern) {
           next ERROR;}
        }
      print "\n";
      print "* module $name\n";
      print "$errmsg\n";
    }
}

elsif ($flag eq "linux64nsyt") {
 if (/Error 1/||/No rule/) {
         $errmsg = $_;
         foreach $pattern (@exceptlinux64nsyt) {
           if ($name =~ $pattern) {
           next ERROR;}
        }
      print "\n";
      print "* module $name\n";
      print "$errmsg\n";
    }
}


elsif ($flag eq "linux32msl") {
 if (/Error 1/||/No rule/) {
         $errmsg = $_;
         foreach $pattern (@exceptlinux32msl) {
           if ($name =~ $pattern) {
           next ERROR;}
        }
      print "\n";
      print "* module $name\n";
      print "$errmsg\n";
    }
}

elsif ($flag eq "linux32nsyt") {
 if (/Error 1/||/No rule/) {
         $errmsg = $_;
         foreach $pattern (@exceptlinux32nsyt) {
           if ($name =~ $pattern) {
           next ERROR;}
        }
      print "\n";
      print "* module $name\n";
      print "$errmsg\n";
    }
}
}
close(LOG);
