// %M% %I% %E% %U%
/** \file
// Scale summitt match results to compensate for stretched model,
// producing equivalent match transform (object->world) for original model.
// orig_match = (est_o2w) * (stretch) * (est_o2w)-1 * (stretch_match) * (stretch)-1
*/
#include "grape/object.h"

static const char usage[] = 
"Usage: %s -e tx ty tz txr tyr tzr -s xscale yscale zscale\n"
"  -t mx my mz -r mxr myr mzr\n\n"
"Writes revised match parameters to standard output.\n"
"-e = original/estimated translation and rotation to world space\n"
"     as used when stretching the model\n"
"-s = stretch factors used for matching model\n"
"-t = match result translation for stretched model\n"
"-r = match result rotation for stretched model\n";

static int got_e, got_s, got_t, got_r;
static double xscale, yscale, zscale;
static ObjNode node1;	// for original/estimated object-world transform
static ObjNode node2;	// for match/refined object-to-world transform

int main (int argc, char **argv)
{
        int i;
        
        for(i=1; i<argc; i++) {
                if (!strcmp(argv[i], "-e")) {
                        got_e = TRUE;
                        node1.x = atof(argv[++i]);
                        node1.y = atof(argv[++i]);
                        node1.z = atof(argv[++i]);
                        node1.xrot = atof(argv[++i]);
                        node1.yrot = atof(argv[++i]);
                        node1.zrot = atof(argv[++i]);
                } else if (!strcmp(argv[i], "-s")) {
                        got_s = TRUE;
                        xscale = atof(argv[++i]);
                        yscale = atof(argv[++i]);
                        zscale = atof(argv[++i]);
                } else if (!strcmp(argv[i], "-t")) {
                        got_t = TRUE;
                        node2.x = atof(argv[++i]);
                        node2.y = atof(argv[++i]);
                        node2.z = atof(argv[++i]);
                } else if (!strcmp(argv[i], "-r")) {
                        got_r = TRUE;
                        node2.xrot = atof(argv[++i]);
                        node2.yrot = atof(argv[++i]);
                        node2.zrot = atof(argv[++i]);
                } else if (!strcmp(argv[i], "-v")) {
                        ; // silently ignore
                } else {
                        fprintf(stderr, "Unknown argument %s\n", argv[i]);
                        fprintf(stderr, usage, argv[0]);
                        return 1;
                }
        }

        if (!(got_e && got_s && got_t && got_r)) {
                fprintf(stderr, "Required argument(s) missing\n");
                fprintf(stderr, usage, argv[0]);
                return 1;
        }

        // original estimated object-to-world transform
        // (as used when stretching the model)
        ZMatrix est_o2w;
        node1.GetObjToWorldTransform(est_o2w);

        // inverse transform back to object space
        ZMatrix est_w2o;
        MatInvert(est_o2w, est_w2o);

        // stretch transform
        ZMatrix stretch;
	MakeScaleMatrix(stretch, xscale, yscale, zscale);

        // scaled match results object-to-world
        ZMatrix smatch;
        node2.GetObjToWorldTransform(smatch);

	// unstretch matched world
	ZMatrix unstretch;
	MakeScaleMatrix(unstretch, 1.0/xscale, 1.0/yscale, 1.0/zscale);

	// accumulate corrected match result
	ZMatrix match;

#if 1	// wrong
	// (((est_o2w * stretch) * est_w2o) * smatch) * unstretch
	MatCopy(est_o2w, match);
	MatMult(match, stretch);
	MatMult(match, est_w2o);
	MatMult(match, smatch);
	MatMult(match, unstretch);
#endif
#if 0	// maybe
	// (((unstretch * smatch) * est_w2o) * stretch) * est_o2w
	MatCopy(unstretch, match);
	MatMult(match, smatch);
        MatMult(match, est_w2o);
        MatMult(match, stretch);
        MatMult(match, est_o2w);
#endif
#if 0	// wrong
	// est_o2w * (stretch * (est_w2o * (smatch * unstretch)))
	MatMult(smatch, unstretch);
	MatMult(est_w2o, smatch);
	MatMult(stretch, est_w2o);
	MatMult(est_o2w, stretch);
	MatCopy(est_o2w, match);
#endif
#if 0
	// ** correct one
	// unstretch * (smatch * (est_w2o * (stretch * est_o2w)))
	MatMult(stretch, est_o2w);
	MatMult(est_w2o, stretch);
	MatMult(smatch, est_w2o);
	MatMult(unstretch, smatch);
	MatCopy(unstretch, match);
#endif

	// MatDump(stderr, "match", match);
	
	// extract translation from matrix
	double tx = match[0][3];
	double ty = match[1][3];
	double tz = match[2][3];
	
	// extract rotations from matrix
	double ry = -asin(match[2][0]);
	double cry = cos(ry);
	double rx =  asin(match[2][1] / cry);
	double rz =  acos(match[0][0] / cry);
	if (cry * cos(rx) * match[2][2] < 0.0)
		rx = M_PI - rx;
	if (cry * sin(rz) * match[1][0] < 0.0)
		rz = -rz;

        // write result
        printf("-t %f %f %f -r %f %f %f\n", tx, ty, tz,
                rx*TODEGREES, ry*TODEGREES, rz*TODEGREES);
                
#if 0
	// ** test
	double p[] = {123.45, -567.89, 32.33};	// object coords
	double pw[3], pws[3];
        node1.GetObjToWorldTransform(est_o2w);
	MultPoints(p, est_o2w, pws);
	pws[0] *= xscale;
	pws[1] *= yscale;
	pws[2] *= zscale;
        MatInvert(est_o2w, est_w2o);
	MultPoints(pws, est_w2o, pw);
        node2.GetObjToWorldTransform(smatch);
	MultPoints(pw, smatch, pws);
	printf("Orig obj pt = %f %f %f\n", p[0], p[1], p[2]);
	printf("Scaled match world   = %f %f %f\n", pws[0], pws[1], pws[2]);
	
	MultPoints(p, match, pw);
	printf("Adjusted match world = %f %f %f\n", pw[0], pw[1], pw[2]);
#endif

        return 0;
}

