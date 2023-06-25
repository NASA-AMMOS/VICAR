#include "dataport.h"
#include "grape/parameter.h"
#include "grape/surface.h"

int GrapeSurface::parse_in(Dataport *fp)
{
	Dataport    *tfp, *tmfp;
	char    token[4096], *ref;

								// assume that the initial tag 'GRAPE_SURFACE'
								// has already been parsed-in


    if(!get_next_token(fp, token)) {
		fprintf(stderr," Whoops - Unexpected EOF in grape surface file\n");
		return(FALSE);
	}

	if ( !strcmp( token, "{")) { // inline surface
		FATAL_ERROR( "NYI");
	}
	else {						// must be a file reference
		
        if ((tfp = dataport_create(fp->get_type())) == NULL) {
			fprintf(stderr," Unable to create dataport\n");
			return FALSE;
		}
		if (!tfp->ropen(token)) {
			fprintf(stderr," Unable to open grape surface file '%s'\n", token);
			return FALSE;
		}

		if ((ref = strdup( token)) == NULL) {
			FATAL_ERROR( "Unable to strdup( token)");
			return FALSE;
		}

		TextureMap *pNewTM;
		while (1) {
			if(!get_next_token(tfp, token)) {
				fprintf(stderr," Whoops - Unexpected EOF in grape surface file\n");
				return(FALSE);
			}

			if (!strcmp( token, "END_GRAPE_SURFACE"))
				break;
			
			if ( !strcmp( token, "COLOR")) {
				if ((pNewTM = new TextureMap()) == NULL) {
					fprintf(stderr," Unable to create color texture map\n");
					return FALSE;
				}

				if ((tmfp = dataport_create(fp->get_type())) == NULL) {
					fprintf(stderr," Unable to create dataport\n");
					return FALSE;
				}

				if(!get_next_token(tfp, token)) {
					fprintf(stderr," Whoops - Unexpected EOF in grape surface file\n");
					return(FALSE);
				}
					
				if (!tmfp->ropen(token)) {
					fprintf(stderr," Unable to open color tmap file '%s'\n",
token);
					return FALSE;
				}

				if (!pNewTM->parse_in( tmfp)) {
					fprintf(stderr," Unable to parse color tmap file '%s'\n",
							token);
					return FALSE;
				}
				set_tmap_color( pNewTM);
			}
			else if ( !strcmp( token, "DISPLACEMENT")) {
				if ((pNewTM = new TextureMap()) == NULL) {
					fprintf(stderr," Unable to create disp texture map\n");
					return FALSE;
				}

				if ((tmfp = dataport_create(fp->get_type())) == NULL) {
					fprintf(stderr," Unable to create dataport\n");
					return FALSE;
				}

				if(!get_next_token(tfp, token)) {
					fprintf(stderr," Whoops - Unexpected EOF in grape surface file\n");
					return(FALSE);
				}
					
				if (!tmfp->ropen(token)) {
					fprintf(stderr," Unable to open disp tmap file '%s'\n",
token);
					return FALSE;
				}

				if (!pNewTM->parse_in( tmfp)) {
					fprintf(stderr," Unable to parse disp tmap file '%s'\n",
							token);
					return FALSE;
				}
				set_tmap_disp( pNewTM);
			}
			else if ( !strcmp( token, "REFLECTIVITY")) {
				if ((pNewTM = new TextureMap()) == NULL) {
					fprintf(stderr," Unable to create refl texture map\n");
					return FALSE;
				}

				if ((tmfp = dataport_create(fp->get_type())) == NULL) {
					fprintf(stderr," Unable to create dataport\n");
					return FALSE;
				}

				if(!get_next_token(tfp, token)) {
					fprintf(stderr," Whoops - Unexpected EOF in grape surface file\n");
					return(FALSE);
				}
					
				if (!tmfp->ropen(token)) {
					fprintf(stderr," Unable to open refl tmap file '%s'\n",
token);
					return FALSE;
				}

				if (!pNewTM->parse_in( tmfp)) {
					fprintf(stderr," Unable to parse refl tmap file '%s'\n",
							token);
					return FALSE;
				}
				set_tmap_refl( pNewTM);
			}
			else {
				fprintf(stderr," Whoops - Unknown surface property '%s'\n",
						token);
				return FALSE;
			}
		}
		set_reference( ref);
		free( ref);
	}

	return TRUE;
	
}

int GrapeSurface::parse_out(Dataport *fp, int expand)
{
	put_token(fp, "\nGRAPE_SURFACE");

	if(expand || !get_reference() ) {
		put_token(fp, " {");

		if ( get_tmap_color() != NULL) {
			put_token( fp, "COLOR");
			get_tmap_color()->parse_out( fp, expand);
		}

		if ( get_tmap_disp() != NULL) {
			put_token( fp, "DISPLACEMENT");
			get_tmap_disp()->parse_out( fp, expand);
		}

		if ( get_tmap_refl() != NULL) {
			put_token( fp, "REFLECTIVITY");
			get_tmap_refl()->parse_out( fp, expand);
		}

		put_token(fp, "\n}");
	} else {
		put_token(fp, get_reference());
	}
	
	put_token(fp, "\nEND_GRAPE_SURFACE");

	return TRUE;
}

int GrapeSurface::get_changed()
{
register int	c;

if(changed==CHANGE_COUNTER) return changed;

if (pColor != NULL)
	if ((c=pColor->get_changed()) > changed)
		changed = c;
if (pDisp != NULL)
	if ((c=pDisp->get_changed()) > changed)
		changed = c;
if (pRefl != NULL)
	if ((c=pRefl->get_changed()) > changed)
		changed = c;

return changed;
}
