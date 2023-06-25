// fstfilter.C 1.9 03/11/14 14:44:40
/** \file
 ** Copy selected terrain patches from a forest to a subforest
 ** for terrain product generation. Selection includes intersecting
 ** volume and boolean logic on fields from XYZ file PDS header
 ** (e.g. source instrument or site ID).
 ** Also optionally update site vectors.
 **/
#include <ctype.h>
#include <string.h>
#include <float.h>
#include <tcl.h>
#include "image/image.h"
#include "image/types/all.h"
#include "summitt_func.h"

static const char usage[] = 
"Usage: %s [-v] [-dir dname] [-r xmin xmax ymin ymax zmin zmax]\n"
"  [-f 'filter_expression'] [-svf site_file]\n"
"  [-i input_forest] -o output_forest\n"
"Filters input forest (default stdin) using intersecting volume and\n"
"filter expression to produce output subforest ('-' for stdout).\n"
"-v   = verbose output\n"
"-dir = directory containing (links to) XYZ files for each forest\n"
"       patch, where XYZ filename is patch name\n"
"-r   = restrict output to component patches that contain data within\n"
"       the specified limits in world coordinates\n"
"-f   = apply boolean selection expression to forest patches\n"
"       (-help for help)\n"
"-svf = replace site vectors with revised data from preprocessed SVF\n";

static const char filter_fmt[] = 
"Filter expression is TCL script, typically:\n"
" <expr>     := <and_expr>\n"
"               <expr> || <and_expr>\n"
" <and_expr> := <cmp_expr>\n"
"               <and_expr> && <cmp_expr>\n"
" <cmp_expr> := <name> == <value>\n"
"               <name> != <value>\n"
"               <name> < <value>\n"
"               <name> <= <value>\n"
"               <name> > <value>\n"
"               <name> >= <value>\n"
"               (<expr>)\n"
"               [string match \"<glob-pattern>\" <name>]\n"
" <name> := %<keyword> | %<group>/<keyword> | $name | $id | $site\n"
"               <keyword> = XYZ file PDS header keyword value\n"
"               $name is patch name (PRODUCT_ID)\n"
"               $id is patch index (0-n) in input forest\n"
"		$site is patch site number\n"
" <value> := \"string\" | <number>\n"
"\nexample: -f '%INSTRUMENT_ID == \"PANCAM_LEFT\" && \n"
" %DERIVED_IMAGE_PARMS/REFERENCE_COORD_SYSTEM_INDEX < 2'\n";

static Forest forest;		// input/output forest

static char *infile, *outfile;	// filenames
static char *filter;		// filter expression
static char *svf_file;		// new site vector preprocessed file
static Summitt_range range;	// bounding volume
static Tcl_Interp *interp;	// expression interpreter

typedef double Vector[3];
static int num_sites;		///< number of site vectors loaded (if any)
static Vector *site_table;	///< table of site vectors

// Directory where XYZ data is (or, more likely, containing links
// to XYZ files). Files must be named the same as patch names.
static char *xyzdir = "../xyz";

// PDS header keywords from filter expression
class Keyword {
public:
	char *group;		// group name, or ""
	char *field;		// field name
	Keyword *next;		// list link
	
	Keyword(char *g, char *f) {	// constructor
		group = strdup(g);
		field = strdup(f);
	}
};

Keyword *keylist;		// head of linked list

/// Is group+keyword already in keylist?
static Keyword *find_keyword(char *g, char *k)
{
	for (Keyword *kp = keylist; kp; kp = kp->next) {
		if (!strcmp(kp->field, k) && !strcmp(kp->group, g))
			return kp;
	}
	return NULL;
}

/// Scan filter expression for PDS keywords, build list
static void scan_filter()
{
	char *s, *delim, *group;
	
	for (s=filter; *s; s=delim) {
		// next candidate
		char *keystart = strchr(s, '%');
		if (keystart == NULL)
			break;		// no more keywords

		// scan past alphanumeric or _
		delim = keystart+1;
		while (*delim == '_' || isalnum(*delim))
			delim++;
		if (delim == keystart+1)
			continue;	// not valid name

		// alter filter string to be TCL variable reference
		*keystart = '$';
		
		// if delimeter is '/', have group + keyword
		if (*delim == '/') {
			group = keystart+1;
			*delim = '_';	// alter filter to "group_key"
			keystart = delim;
			// scan for keyword
			while (*delim == '_' || isalnum(*delim))
				delim++;
		} else {
			group = "";
		}
		
		// allocate keyword on list if new
		char save1 = *keystart; *keystart = 0;
		char save2 = *delim; *delim = 0;
		if (!find_keyword(group, keystart+1)) {
			Keyword *k = new Keyword(group, keystart+1);
			k->next = keylist; keylist = k;
		}
		*keystart = save1;
		*delim = save2;
	}
	
	if (verbose > 1) {	// debug
		fprintf(stderr, "Post-scan filter: %s\n", filter);
		fprintf(stderr, "Keylist:\n");
		for (Keyword *k = keylist; k; k=k->next)
			fprintf(stderr, " %s -> %s\n", k->group, k->field);
	}
}

/// Return TRUE if patch matches filter expression
static int passes_filter(Patch *p, int id)
{
	char fcmd[2048];

	// set Tcl variable 'name' to patch name, 'id' to patch number,
	// 'site' to site number
	sprintf(fcmd, "set name %s\nset id %d\nset site %d\n", 
		p->get_name(), id, p->site);
	if (Tcl_Eval(interp, fcmd) != TCL_OK) {
		fprintf(stderr, "Whoops - problem setting patch fields: %s\n",
			interp->result);
		return 1;	// pass patch on error
	}

	// set Tcl variables for keywords from PDS headers
	if (keylist) {
		// read XYZ file's PDS header
		sprintf(fcmd, "%s/%s", xyzdir, p->get_name());
	        Image img;
		PDSFile *pds = new PDSFile;
		pds->set_image(&img);
 		if (pds->read_header(fcmd)) {
			fprintf(stderr, "%s not found or not a valid PDS\n",
                        	fcmd);
                	delete pds;
                	return 0;
                }

		for (Keyword *k = keylist; k; k=k->next) {
			char *val = pds->get_value(k->field, 
					k->group[0] ? k->group : NULL);
			if (val == NULL) {	// field not found
				if (k->group[0])
					sprintf(fcmd, "catch {unset %s_%s}\n",
						k->group, k->field);
				else
					sprintf(fcmd, "catch {unset %s}\n", 
						k->field);
			} else {		// found, set value
				if (k->group[0])
					sprintf(fcmd, "set %s_%s %s\n",
						k->group, k->field, val);
				else
					sprintf(fcmd, "set %s %s\n", 
						k->field, val);
			}
			if (verbose > 1)
				fprintf(stderr, "TCL eval: %s", fcmd);
			if (Tcl_Eval(interp, fcmd) != TCL_OK) {
				fprintf(stderr, "Whoops - problem setting"
					" patch fields: %s\n",
					interp->result);
				delete pds;
				return 1;	// pass patch on error
			}
		}
		delete pds;
	}

	// evaluate filter expression
	int result;
	if (Tcl_ExprBoolean(interp, filter, &result) != TCL_OK) {
		fprintf(stderr, "Invalid filter expression, %s\n",
			interp->result);
		return 1;	// pass the patch anyway
	}

	return result;
}

/// Load site vector data preprocessed by site_vector.p into text format
// (to simplify this app and isolate it from SVF format changes)
static void svf_load(const char *svf_file)
{
	FILE *fp = fopen(svf_file, "r");
	if (fp == NULL) {
		fprintf(stderr, "Can't open SVF temp file %s\n", svf_file);
		exit(1);
	}

	char buf[256];
	while (fgets(buf, sizeof(buf), fp)) {
		int site;
		double x, y, z;
		if (sscanf(buf, "%d %lf %lf %lf", &site, &x, &y, &z) != 4) {
			fprintf(stderr, "SVF temp file %s invalid at %s\n",
				svf_file, buf);
			exit(1);
		}
		// last site is listed first, to set allocation
		if (site >= num_sites) {
			num_sites = site+1;
			delete [] site_table;	// just in case
			site_table = new Vector[num_sites];
		}

		site_table[site][0] = x;
		site_table[site][1] = y;
		site_table[site][2] = z;
	}
	fclose(fp);
}

// update site vector data for this patch
static void update_site(Patch *p)
{
	// new site vector (world->site)
	double *new_sv = site_table[p->site];

	// Remove old site-to-world translation, apply new one.
	// Translate bounding volume too.
	double d = p->site_vector[0] - new_sv[0];
	p->x = p->x + d;
	p->bvol.xmin += d;
	p->bvol.xmax += d;

	d = p->site_vector[1] - new_sv[1];
	p->y = p->y + d;
	p->bvol.ymin += d;
	p->bvol.ymax += d;

	d = p->site_vector[2] - new_sv[2];
	p->z = p->z + d;
	p->bvol.zmin += d;
	p->bvol.zmax += d;

	// store new site vector in patch
	vector_copy(new_sv, p->site_vector);
}

int main(int argc, char **argv)
{
	int i;

	for (i=1; i<argc; i++) {
		if(!strcmp(argv[i],"-i") && i+i < argc) {
			infile = argv[++i];
		} else if(!strcmp(argv[i],"-o") && i+1 < argc) {
			outfile = argv[++i];
		} else if(!strcmp(argv[i],"-f") && i+1 < argc) {
			filter = argv[++i];
		} else if(!strcmp(argv[i],"-v")) {
			verbose++;
		} else if(!strcmp(argv[i],"-r") && i+6<argc) {
			range.space = SUMMITT_OBJECT_SPACE;
			range.xmin = atof(argv[++i]);
			range.xmax = atof(argv[++i]);
			range.ymin = atof(argv[++i]);
			range.ymax = atof(argv[++i]);
			range.zmin = atof(argv[++i]);
			range.zmax = atof(argv[++i]);
		} else if(!strcmp(argv[i],"-dir") && i+1 < argc) {
			xyzdir = argv[++i];
		} else if(!strcmp(argv[i],"-svf") && i+1 < argc) {
			svf_file = argv[++i];
		} else if(!strcmp(argv[i],"-help")) {
			fprintf(stderr, filter_fmt);
			exit(1);
		} else {
			fprintf(stderr, "Unknown argument %s\n", argv[i]);
			fprintf(stderr, usage, argv[0]);
			exit(1);
		}
	}

	if (outfile == NULL) {
		fprintf(stderr, "Required argument missing\n");
		fprintf(stderr, usage, argv[0]);
		exit(1);
	}

	if (filter) {
		// setup Tcl interpreter for evaluating filter expression
		interp = Tcl_CreateInterp();
		Tcl_Init(interp);

		// scan filter expression for PDS keywords
		scan_filter();
	}

	// read in forest
	FILE_Dataport fp;
	if(infile) {
		if (!fp.ropen(infile)) {
			fprintf(stderr, 
				"%s: Whoops - Can't open %s for reading\n", 
				argv[0], infile);
			exit(1);
		}
	} else {
		fp.open(stdin);
		infile = (char *)"stdin";
	}

	if (verbose)
		fprintf(stderr,"Loading input data\n");

 	char token[4096];
 	get_next_token(&fp, token);
	if (strcmp(token, "GRP_V1")) {
		fprintf(stderr, "%s: input %s isn't a forest\n",
			argv[0], infile);
		exit(1);
	}

	// load forest, but no patch data yet
	if (!forest.parse_in(&fp, FALSE))
		exit(1);
	fp.close();

	if (verbose)
		fprintf(stderr,"Input %s has %d patch(es)\n", 
			infile, forest.get_num_children());

	// if site vector file specified, load site vector table
	if (svf_file) {
		svf_load(svf_file);
		if (verbose)
			fprintf(stderr, "Loaded %d site vectors from %s\n",
				num_sites, svf_file);
	}

	// loop through forest patches
	i = 0;		// original patch index
	for (int tree=0; tree<forest.get_num_children(); ) {
		Patch *p = forest.get_patch(tree);
		if (verbose)
			fprintf(stderr, "\nChecking patch %s\n", p->get_name());

		if (range.space && !range.overlaps(&p->bvol)) {
			if (verbose)
				fprintf(stderr, "No overlap with volume\n");
			forest.remove_child(tree);
			continue;
		}

		if (filter && !passes_filter(p, i)) {
			if (verbose)
				fprintf(stderr, "Excluded by filter\n");
			forest.remove_child(tree);
			continue;
		}

		// keep this patch; update site vector if requested
		if (site_table)
			update_site(p);
		tree++;
		if (verbose)
			fprintf(stderr, "Passes filter\n");
	}

	// write output forest	
	if (!strcmp(outfile, "-"))
		fp.open(stdout);
	else if (!fp.wopen(outfile)) {
		fprintf(stderr, "%s: Whoops - Can't open %s for writing\n", 
				argv[0], outfile);
		exit(1);
	}
	forest.parse_out(&fp);
	if (verbose)
		fprintf(stderr, "Output %s has %d patch(es)\n",
			outfile, forest.get_num_children());

	return 0;
}
