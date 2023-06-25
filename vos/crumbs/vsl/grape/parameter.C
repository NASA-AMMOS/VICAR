// parameter.C

#include "grape/parameter.h"
#include "grape/d_envelope.h"
#include "image/image.h"


Data	*dparam::envelope_create(Dataport *fp)
{
	char    token[4096];
	dData	*temp=NULL;

	if(!get_next_token(fp, token)) {
		fprintf(stderr," Whoops - Unexpected EOF in parsing double parameter\n");
		return(FALSE);
	}
	if(!strcmp(token, "ENV_DDE_V1")) {
		temp = new dEdigital();
	} else if(!strcmp(token, "ENV_DLE_V1")) {
		temp = new dElinear();
	} else if(!strcmp(token, "WVSPLINE_V1")) {
		temp = new dEspline();
	} else if(!strcmp(token, "SURVSPLINE_V1")) {
		temp = new dEsurvspline();
	} else {
		fprintf(stderr,"Whoops - Unknown envelope header encountered = >%s<\n", token);
	}
	return(temp);
}

int  dparam::parse_in(Dataport *fp)
{
	char	token[4096];
	Data	*env;
	Dataport	*tfp;

	if(!get_next_token(fp, token)) {
		fprintf(stderr," Whoops - Unexpected EOF in parsing double parameter\n");
		return(FALSE);
	}
	if(!strcmp(token, "DATA")) {
		if(!get_next_token(fp, token)) {
			fprintf(stderr," Whoops - Unexpected EOF in scene file\n");
			return(FALSE);
		}
		if(!strcmp(token, "{")) {
			env = envelope_create(fp);
			if(!env) {
				fprintf(stderr,"Whoops - Unrecognized envelope type in scene file %s\n",token);
				return(FALSE);
			}
			if(!env->parse_in(fp)) {
				fprintf(stderr,"Whoops - Problems parsing referenced envelope %s\n",token);
				return(FALSE);
			}
			if(!get_next_token(fp, token)) {
				fprintf(stderr," Whoops - Unexpected EOF in scene file\n");
				return(FALSE);
			}
			if(strcmp(token, "}")) {
				fprintf(stderr," Whoops - Missing } after inline envelope \n");
				return(FALSE);
			}
		} else {
			tfp = dataport_create(fp->get_type());
			if(tfp && tfp->ropen(token)) {
				env = envelope_create(tfp);
				if(!env) {
					fprintf(stderr,"Whoops - Unrecognized envelope type in external object file %s\n",token);
					return(FALSE);
				}
				if(!env->parse_in(tfp)) {
					fprintf(stderr,"Whoops - Problems parsing referenced envelope %s\n",token);
					return(FALSE);
				}
				env->set_reference(token);
			} else {
				fprintf(stderr,"Whoops - Unable to open referenced envelope file %s\n", token);
				return(FALSE);
			}
		}
		set_data(env);
	} else {
		set_value(atof(token));
	}
	return(TRUE);
}

int  dparam::parse_out(Dataport *fp, int expand)
{
	char	token[4096];

	if(dat) {
		put_token(fp, "\nDATA");
		if(expand || !dat->get_reference()) {
			put_token(fp, "{");
			dat->parse_out(fp, expand);
			put_token(fp, "}");
		} else {
			put_token(fp, dat->get_reference());
		}
	} else {
		sprintf(token,"%f", get_value());
		put_token(fp, token);
	}
	return(TRUE);
}

int  iparam::parse_in(Dataport *fp)
{
	char	token[4096];
	Data	*env;
	Dataport	*tfp;

	if(!get_next_token(fp, token)) {
		fprintf(stderr," Whoops - Unexpected EOF in parsing integer parameter\n");
		return(FALSE);
	}
	if(!strcmp(token, "DATA")) {
		if(!get_next_token(fp, token)) {
			fprintf(stderr," Whoops - Unexpected EOF in scene file\n");
			return(FALSE);
		}
		if(!strcmp(token, "{")) {
			env = envelope_create(fp);
			if(!env) {
				fprintf(stderr,"Whoops - Unrecognized envelope type in scene file %s\n",token);
				return(FALSE);
			}
			if(!env->parse_in(fp)) {
				fprintf(stderr,"Whoops - Problems parsing referenced envelope %s\n",token);
				return(FALSE);
			}
			if(!get_next_token(fp, token)) {
				fprintf(stderr," Whoops - Unexpected EOF in scene file\n");
				return(FALSE);
			}
			if(strcmp(token, "}")) {
				fprintf(stderr," Whoops - Missing } after inline envelope \n");
				return(FALSE);
			}
		} else {
			tfp = dataport_create(fp->get_type());
			if(tfp && tfp->ropen(token)) {
				env = envelope_create(tfp);
				if(!env) {
					fprintf(stderr,"Whoops - Unrecognized envelope type in external object file %s\n",token);
					return(FALSE);
				}
				if(!env->parse_in(tfp)) {
					fprintf(stderr,"Whoops - Problems parsing referenced envelope %s\n",token);
					return(FALSE);
				}
				env->set_reference(token);
			} else {
				fprintf(stderr,"Whoops - Unable to open referenced envelope file %s\n", token);
				return(FALSE);
			}
		}
		set_data(env);
	} else {
		set_value(atoi(token));
	}
	return(TRUE);
}

int  iparam::parse_out(Dataport *fp, int expand)
{
	char	token[4096];

	if(dat) {
		put_token(fp, "\nDATA");
		if(expand || !dat->get_reference()) {
			put_token(fp, "{");
			dat->parse_out(fp, expand);
			put_token(fp, "}");
		} else {
			put_token(fp, dat->get_reference());
		}
	} else {
		sprintf(token,"%d", get_value());
		put_token(fp, token);
	}
	return(TRUE);
}

int  ldparam::parse_in(Dataport *fp)
{
	char	token[4096];
	Data	*env;
	Dataport	*tfp;
	int	i, count;

	if(!get_next_token(fp, token)) {
		fprintf(stderr," Whoops - Unexpected EOF in parsing double list parameter\n");
		return(FALSE);
	}
	if(!strcmp(token, "LIST")) {
		if(!get_next_token(fp, token)) {
			fprintf(stderr," Whoops - Unexpected EOF in scene file\n");
			return(FALSE);
		}
		count = atoi(token);

		free_list();

		for(i=0; i<count; i++) {
			if(!get_next_token(fp, token)) {
				fprintf(stderr," Whoops - Unexpected EOF in parsing double list parameter\n");
				return(FALSE);
			}
			if(!strcmp(token, "DATA")) {
				if(!get_next_token(fp, token)) {
					fprintf(stderr," Whoops - Unexpected EOF in scene file\n");
					return(FALSE);
				}
				if(!strcmp(token, "{")) {
					env = envelope_create(fp);
					if(!env) {
						fprintf(stderr,"Whoops - Unrecognized envelope type in scene file %s\n",token);
						return(FALSE);
					}
					if(!env->parse_in(fp)) {
						fprintf(stderr,"Whoops - Problems parsing referenced envelope %s\n",token);
						return(FALSE);
					}
					if(!get_next_token(fp, token)) {
						fprintf(stderr," Whoops - Unexpected EOF in scene file\n");
						return(FALSE);
					}
					if(strcmp(token, "}")) {
						fprintf(stderr," Whoops - Missing } after inline envelope \n");
						return(FALSE);
					}
				} else {
					tfp = dataport_create(fp->get_type());
					if(tfp && tfp->ropen(token)) {
						env = envelope_create(tfp);
						if(!env) {
							fprintf(stderr,"Whoops - Unrecognized envelope type in external object file %s\n",token);
							return(FALSE);
						}
						if(!env->parse_in(tfp)) {
							fprintf(stderr,"Whoops - Problems parsing referenced envelope %s\n",token);
							return(FALSE);
						}
						env->set_reference(token);
					} else {
						fprintf(stderr,"Whoops - Unable to open referenced envelope file %s\n", token);
						return(FALSE);
					}
				}
				set_data(env);
			} else {
				add_value(atof(token));
			}
		}
	} else {
		fprintf(stderr,"Whoops - Unexpected token >%s< encountered in double list parameter - Expecting LIST\n", token);
		return(FALSE);
	}
	return(TRUE);
}

int  ldparam::parse_out(Dataport *fp, int expand)
{
	char	token[4096];
	int	i;

	put_token(fp, "\nLIST");
	sprintf(token,"%d", get_num());
	put_token(fp, token);
	for(i=0; i<get_num(); i++) {
		if(dat) {
			put_token(fp, "\nDATA");
			if(expand || !dat->get_reference()) {
				put_token(fp, "{");
				dat->parse_out(fp, expand);
				put_token(fp, "}");
			} else {
				put_token(fp, dat->get_reference());
			}
		} else {
			sprintf(token,"%f", get_value(i));
			put_token(fp, token);
		}
	}
	return(TRUE);
}

int  liparam::parse_in(Dataport *fp)
{
	char	token[4096];
	Data	*env;
	Dataport	*tfp;
	int	i, count;

	if(!get_next_token(fp, token)) {
		fprintf(stderr," Whoops - Unexpected EOF in parsing integer list parameter\n");
		return(FALSE);
	}
	if(!strcmp(token, "LIST")) {
		if(!get_next_token(fp, token)) {
			fprintf(stderr," Whoops - Unexpected EOF in scene file\n");
			return(FALSE);
		}
		count = atoi(token);

		for(i=0; i<count; i++) {
			if(!get_next_token(fp, token)) {
				fprintf(stderr," Whoops - Unexpected EOF in parsing integer list parameter\n");
				return(FALSE);
			}
			if(!strcmp(token, "DATA")) {
				if(!get_next_token(fp, token)) {
					fprintf(stderr," Whoops - Unexpected EOF in scene file\n");
					return(FALSE);
				}
				if(!strcmp(token, "{")) {
					env = envelope_create(fp);
					if(!env) {
						fprintf(stderr,"Whoops - Unrecognized envelope type in scene file %s\n",token);
						return(FALSE);
					}
					if(!env->parse_in(fp)) {
						fprintf(stderr,"Whoops - Problems parsing referenced envelope %s\n",token);
						return(FALSE);
					}
					if(!get_next_token(fp, token)) {
						fprintf(stderr," Whoops - Unexpected EOF in scene file\n");
						return(FALSE);
					}
					if(strcmp(token, "}")) {
						fprintf(stderr," Whoops - Missing } after inline envelope \n");
						return(FALSE);
					}
				} else {
					tfp = dataport_create(fp->get_type());
					if(tfp && tfp->ropen(token)) {
						env = envelope_create(tfp);
						if(!env) {
							fprintf(stderr,"Whoops - Unrecognized envelope type in external object file %s\n",token);
							return(FALSE);
						}
						if(!env->parse_in(tfp)) {
							fprintf(stderr,"Whoops - Problems parsing referenced envelope %s\n",token);
							return(FALSE);
						}
						env->set_reference(token);
					} else {
						fprintf(stderr,"Whoops - Unable to open referenced envelope file %s\n", token);
						return(FALSE);
					}
				}
				set_data(env);
			} else {
				add_value(atoi(token));
			}
		}
	} else {
		fprintf(stderr,"Whoops - Unexpected token >%s< encountered in integer list parameter - Expecting LIST\n", token);
		return(FALSE);
	}
	return(TRUE);
}

int  liparam::parse_out(Dataport *fp, int expand)
{
	char	token[4096];
	int	i;

	put_token(fp, "\nLIST");
	sprintf(token,"%d", get_num());
	put_token(fp, token);
	for(i=0; i<get_num(); i++) {
		if(dat) {
			put_token(fp, "\nDATA");
			if(expand || !dat->get_reference()) {
				put_token(fp, "{");
				dat->parse_out(fp, expand);
				put_token(fp, "}");
			} else {
				put_token(fp, dat->get_reference());
			}
		} else {
			sprintf(token,"%d", get_value(i));
			put_token(fp, token);
		}
	}
	return(TRUE);
}

Data	*imgparam::envelope_create(Dataport *fp)
{
	char    token[4096];
	imgData	*temp=NULL;

	if(!get_next_token(fp, token)) {
		fprintf(stderr," Whoops - Unexpected EOF in parsing image parameter\n");
		return(FALSE);
	}
	fprintf(stderr,"Whoops - Unknown envelope header encountered = >%s<\n", token);
	return(temp);
}

int  imgparam::parse_in(Dataport *fp)
{
	char	token[4096];
	Data	*env;
	Dataport	*tfp;
	ImageFile	*ifile;

	if(!get_next_token(fp, token)) {
		fprintf(stderr," Whoops - Unexpected EOF in parsing double parameter\n");
		return(FALSE);
	}
	if(!strcmp(token, "DATA")) {
		if(!get_next_token(fp, token)) {
			fprintf(stderr," Whoops - Unexpected EOF in scene file\n");
			return(FALSE);
		}
		if(!strcmp(token, "{")) {
			env = envelope_create(fp);
			if(!env) {
				fprintf(stderr,"Whoops - Unrecognized envelope type in scene file %s\n",token);
				return(FALSE);
			}
			if(!env->parse_in(fp)) {
				fprintf(stderr,"Whoops - Problems parsing referenced envelope %s\n",token);
				return(FALSE);
			}
			if(!get_next_token(fp, token)) {
				fprintf(stderr," Whoops - Unexpected EOF in scene file\n");
				return(FALSE);
			}
			if(strcmp(token, "}")) {
				fprintf(stderr," Whoops - Missing } after inline envelope \n");
				return(FALSE);
			}
		} else {
			tfp = dataport_create(fp->get_type());
			if(tfp && tfp->ropen(token)) {
				env = envelope_create(tfp);
				if(!env) {
					fprintf(stderr,"Whoops - Unrecognized envelope type in external object file %s\n",token);
					return(FALSE);
				}
				if(!env->parse_in(tfp)) {
					fprintf(stderr,"Whoops - Problems parsing referenced envelope %s\n",token);
					return(FALSE);
				}
				env->set_reference(token);
			} else {
				fprintf(stderr,"Whoops - Unable to open referenced envelope file %s\n", token);
				return(FALSE);
			}
		}
		set_data(env);
	} else {
		img=new Image(token);
		ifile=*img;
		if(!ifile) return(FALSE);
		if(ifile->get_error()) return(FALSE);
	}
	return(TRUE);
}

int  imgparam::parse_out(Dataport *fp, int expand)
{
	char	*fn;

	if(dat) {
		put_token(fp, "\nDATA");
		if(expand || !dat->get_reference()) {
			put_token(fp, "{");
			dat->parse_out(fp, expand);
			put_token(fp, "}");
		} else {
			put_token(fp, dat->get_reference());
		}
	} else {
		fn=img->get_filename();
		if(fn) put_token(fp,fn);
		else return(FALSE);
	}
	return(TRUE);
}

