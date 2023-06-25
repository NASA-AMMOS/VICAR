// d_envelope.C

#include "grape/d_envelope.h"

// **********  dEdigital member functions  ***************

void dEdigital::set_value_at_time(double t, double val)
{
int	n;

if(!num) return;
n=(int)((t-start)/delta);
if(n<0) n=0;
else	if(n>=num) n=num-1;
data[n]=val;
changed=CHANGE_COUNTER;
}


double dEdigital::get_value_at_time(double t)
{
int	n;
double	v;

if(num<2) {
	if(!num) return 0.0;
	return data[0];
	}
v=(t-start)/delta;
if(v<0.0) return data[0];
n=(int)v;
if(n>=num-1) return data[num-1];
if(interp) {
	v-=(double)n;
	return data[n]*(1.0-v)+data[n+1]*v;
	}
return	data[n];
}

int     dEdigital::parse_in(Dataport *fp)
{
	char	token[4096];
	int	i, j, temp, stride, skip;
	Dataport	*tfp;

	cleanup();
	init();
	do {
		if(!get_next_token(fp, token)) {
			fprintf(stderr," Whoops - Unexpected EOF in envelope file\n");
			return(FALSE);
		}
		if(!strcmp(token, "VERSION")) {
			if(!get_next_token(fp, token)) {
				fprintf(stderr," Whoops - Unexpected EOF in envelope file\n");
				return(FALSE);
			}
			if(strcmp(token, "1.0")) {
				fprintf(stderr," Whoops - Unexpected version number encountered in parsing envelope object\n");
				fprintf(stderr,"   Token=>%s<  Expecting >1.0<\n", token);
				return(FALSE);
			}
			version = strdup(token);
		} else if(!strcmp(token, "NAME")) {
			if(!get_next_token(fp, token)) {
				fprintf(stderr," Whoops - Unexpected EOF in envelope file\n");
				return(FALSE);
			}
			name = strdup(token);
		} else if(!strcmp(token, "TYPE")) {
			if(!get_next_token(fp, token)) {
				fprintf(stderr," Whoops - Unexpected EOF in envelope file\n");
				return(FALSE);
			}
			if(strcmp(token, "DISCRETE_DOUBLE")) {
				fprintf(stderr," Whoops - Unexpected envelope type encountered in parsing envelope\n");
				fprintf(stderr,"   Token=>%s<  Expecting >DISCRETE_DOUBLE<\n", token);
				return(FALSE);
			}
		} else if(!strcmp(token, "START")) {
			if(!get_next_token(fp, token)) {
				fprintf(stderr," Whoops - Unexpected EOF in envelope file\n");
				return(FALSE);
			}
			set_start(atof(token));
		} else if(!strcmp(token, "DELTA")) {
			if(!get_next_token(fp, token)) {
				fprintf(stderr," Whoops - Unexpected EOF in envelope file\n");
				return(FALSE);
			}
			set_delta(atof(token));
		} else if(!strcmp(token, "VALUES")) {
			if(!get_next_token(fp, token)) {
				fprintf(stderr," Whoops - Unexpected EOF in envelope file\n");
				return(FALSE);
			}
			if(strcmp(token, "REFER")) {
				temp = atoi(token);
				set_num(temp);
				for(i=0; i<temp; i++) {
					if(!get_next_token(fp, token)) {
						fprintf(stderr," Whoops - Unexpected EOF in envelope file\n");
						return(FALSE);
					}
					set_value_at_index(i, atof(token));
				}
			} else {
				stride = 0;
				skip = 0;
				if(!get_next_token(fp, token)) {
					fprintf(stderr," Whoops - Unexpected EOF in envelope file\n");
					return(FALSE);
				}
				tfp = dataport_create(fp->get_type());
				if(tfp && tfp->ropen(token)) {
					do {
						if(!get_next_token(fp, token)) {
							fprintf(stderr," Whoops - Unexpected EOF in envelope file\n");
							return(FALSE);
						}
						if(!strcmp(token, "STRIDE")) {
							if(!get_next_token(fp, token)) {
								fprintf(stderr," Whoops - Unexpected EOF in envelope file\n");
								return(FALSE);
							}
							stride = atoi(token) - 1;
							if(stride < 0)stride = 0;
						} else if(!strcmp(token, "SKIP")) {
							if(!get_next_token(fp, token)) {
								fprintf(stderr," Whoops - Unexpected EOF in envelope file\n");
								return(FALSE);
							}
							skip = atoi(token);
							if(skip < 0)skip = 0;
						} else if(strcmp(token, "ENDREF")) {
							fprintf(stderr," Whoops - Unexpected token encountered in VALUES area = %s\n", token);
							return(FALSE);
						}
					} while(strcmp(token, "ENDREF"));
					for(j=0; j<skip; j++) {
						if(!get_next_token(tfp, token)) {
							break;
						}
					}
					i = 0;
					while(get_next_token(tfp, token)) {
						set_num(i+1);
						set_value_at_index(i++, atof(token));
						for(j=0; j<stride; j++) {
							if(!get_next_token(tfp, token)) {
								break;
							}
						}
					}
					tfp->close();
					delete(tfp);
				} else {
					fprintf(stderr," Whoops - Unable to open external value file %d\n", token);
					return(FALSE);
				}
			}
		} else if(!strcmp(token, "INTERP")) {
			if(!get_next_token(fp, token)) {
				fprintf(stderr," Whoops - Unexpected EOF in envelope file\n");
				return(FALSE);
			}
			if(!strcmp(token, "ON")) {
				set_interpolation(TRUE);
			} else if(!strcmp(token, "OFF")) {
				set_interpolation(FALSE);
			} else {
				fprintf(stderr," Whoops - Unexpected interpolation flag encountered in parsing envelope\n");
				fprintf(stderr,"   Token=>%s<  Expecting >ON or OFF<\n", token);
				return(FALSE);
			}
		} else if(!strcmp(token, "CLOCK")) {
			if(!get_next_token(fp, token)) {
				fprintf(stderr," Whoops - Unexpected EOF in envelope file\n");
				return(FALSE);
			}
			if(!strcmp(token, "NULL")) {
				set_clock(NULL);
			} else if(!strcmp(token, "DEFAULT")) {
				set_clock(&global_clock);
			} else {
				fprintf(stderr," Whoops - Unexpected clock flag encountered in parsing envelope\n");
				fprintf(stderr,"   Token=>%s<  Expecting >NULL, DEFAULT, or name (name not available yet)<\n", token);
				return(FALSE);
			}
	
		} else if(strcmp(token, "END_ENVELOPE")) {
			fprintf(stderr," Whoops - Unexpected token encountered in parsing envelope object\n");
			fprintf(stderr,"   Token=>%s<  Expecting >END_ENVELOPE<\n", token);
			return(FALSE);
		}
	} while (strcmp(token, "END_ENVELOPE"));

	return(TRUE);
}

int     dEdigital::parse_out(Dataport *fp, int )
{
	char    token[4096];
	int     i, temp;

	put_token(fp, "ENV_DDE_V1");
	put_token(fp, "\nVERSION");
	put_token(fp, "1.0");
	if(name) {
		put_token(fp, "\nNAME");
		put_token(fp, name);
	}
	put_token(fp, "\nTYPE");
	put_token(fp, "DISCRETE_DOUBLE");
	put_token(fp, "\nSTART");
	sprintf(token, "%f", get_start());
	put_token(fp, token);
	put_token(fp, "\nDELTA");
	sprintf(token, "%f", get_delta());
	put_token(fp, token);
	put_token(fp, "\nVALUES");
	temp = get_num();
	sprintf(token, "%d\n", temp);
	put_token(fp, token);

	for(i=0; i<temp; i++) {
		sprintf(token, "%f", get_value_at_index(i));
		put_token(fp, token);
	}

	put_token(fp, "\nINTERP");
	if(get_interpolation()) {
		put_token(fp, "ON");
	} else {
		put_token(fp, "OFF");
	}

	put_token(fp, "\nCLOCK");
	if(!get_clock()) {
		put_token(fp, "NULL");
	} else if(get_clock() == &global_clock) {
		put_token(fp, "DEFAULT");
	} else {
//		put_token(fp, clk->get_name());
fprintf(stderr,"Unable to output nondefault clock name yet.\n");
	}

	put_token(fp, "\nEND_ENVELOPE");

	return(TRUE);

}

// **********  dElinear member functions  ***************

void dElinear::set_value_at_time(double t, double val)
{
int	i,j;

if(!num) {
	num=1;
	data=(double *)malloc(sizeof(double));
	time=(double *)malloc(sizeof(double));
	data[0]=val;
	time[0]=t;
	return;
	}
num++;
data=(double *)realloc(data,num*sizeof(double));
time=(double *)realloc(time,num*sizeof(double));
for(i=0;i<num-1;i++) if(time[i]>t) break;
for(j=num-2;j>=i;j--) { data[j+1]=data[j]; time[j+1]=time[j]; }
data[i]=val;
time[i]=t;
}


double dElinear::get_value_at_time(double t)
{
int	i;

if(num<2) {
	if(!num) return 0.0;
	return data[0];
	}
if(t<=time[0]) return data[0];
for(i=1;i<num;i++) if(time[i]>t) break;
if(i==num) return data[num-1];
i--;
if(interp) return data[i]+(data[i+1]-data[i])*(t-time[i])/(time[i+1]-time[i]);
return data[i];
}


void dElinear::delete_index(int i)
{
int	n;

if(i<0 || i>=num) return;
if(num==1) free_data();

for(n=i;n<num-1;n++) { data[n]=data[n+1]; time[n]=time[n+1]; }
num--;
data=(double *)realloc(data,num*sizeof(double));
time=(double *)realloc(time,num*sizeof(double));
}


int     dElinear::parse_in(Dataport *fp)
{
	char	token[4096];
	int	i, temp;
	double	ttime;

	cleanup();
	init();
	do {
		if(!get_next_token(fp, token)) {
			fprintf(stderr," Whoops - Unexpected EOF in envelope file\n");
			return(FALSE);
		}
		if(!strcmp(token, "VERSION")) {
			if(!get_next_token(fp, token)) {
				fprintf(stderr," Whoops - Unexpected EOF in envelope file\n");
				return(FALSE);
			}
			if(strcmp(token, "1.0")) {
				fprintf(stderr," Whoops - Unexpected version number encountered in parsing envelope object\n");
				fprintf(stderr,"   Token=>%s<  Expecting >1.0<\n", token);
				return(FALSE);
			}
			version = strdup(token);
		} else if(!strcmp(token, "NAME")) {
			if(!get_next_token(fp, token)) {
				fprintf(stderr," Whoops - Unexpected EOF in envelope file\n");
				return(FALSE);
			}
			name = strdup(token);
		} else if(!strcmp(token, "TYPE")) {
			if(!get_next_token(fp, token)) {
				fprintf(stderr," Whoops - Unexpected EOF in envelope file\n");
				return(FALSE);
			}
			if(strcmp(token, "DISCRETE_LINEAR_DOUBLE")) {
				fprintf(stderr," Whoops - Unexpected envelope type encountered in parsing envelope\n");
				fprintf(stderr,"   Token=>%s<  Expecting >DISCRETE_LINEAR_DOUBLE<\n", token);
				return(FALSE);
			}
		} else if(!strcmp(token, "VALUES")) {
			if(!get_next_token(fp, token)) {
				fprintf(stderr," Whoops - Unexpected EOF in envelope file\n");
				return(FALSE);
			}
			temp = atoi(token);
			for(i=0; i<temp; i++) {
				if(!get_next_token(fp, token)) {
					fprintf(stderr," Whoops - Unexpected EOF in envelope file\n");
					return(FALSE);
				}
				ttime = atof(token);
				if(!get_next_token(fp, token)) {
					fprintf(stderr," Whoops - Unexpected EOF in envelope file\n");
					return(FALSE);
				}
				set_value_at_time(ttime, atof(token));
			}
		} else if(!strcmp(token, "INTERP")) {
			if(!get_next_token(fp, token)) {
				fprintf(stderr," Whoops - Unexpected EOF in envelope file\n");
				return(FALSE);
			}
			if(!strcmp(token, "ON")) {
				set_interpolation(TRUE);
			} else if(!strcmp(token, "OFF")) {
				set_interpolation(FALSE);
			} else {
				fprintf(stderr," Whoops - Unexpected interpolation flag encountered in parsing envelope\n");
				fprintf(stderr,"   Token=>%s<  Expecting >ON or OFF<\n", token);
				return(FALSE);
			}
		} else if(!strcmp(token, "CLOCK")) {
			if(!get_next_token(fp, token)) {
				fprintf(stderr," Whoops - Unexpected EOF in envelope file\n");
				return(FALSE);
			}
			if(!strcmp(token, "NULL")) {
				set_clock(NULL);
			} else if(!strcmp(token, "DEFAULT")) {
				set_clock(&global_clock);
			} else {
				fprintf(stderr," Whoops - Unexpected clock flag encountered in parsing envelope\n");
				fprintf(stderr,"   Token=>%s<  Expecting >NULL, DEFAULT, or name (name not available yet)<\n", token);
				return(FALSE);
			}
	
		} else if(strcmp(token, "END_ENVELOPE")) {
			fprintf(stderr," Whoops - Unexpected token encountered in parsing envelope object\n");
			fprintf(stderr,"   Token=>%s<  Expecting >END_ENVELOPE<\n", token);
			return(FALSE);
		}
	} while (strcmp(token, "END_ENVELOPE"));

	return(TRUE);
}

int     dElinear::parse_out(Dataport *fp, int )
{
	char    token[4096];
	int     i, temp;

	put_token(fp, "ENV_DLE_V1");
	put_token(fp, "\nVERSION");
	put_token(fp, "1.0");
	if(name) {
		put_token(fp, "\nNAME");
		put_token(fp, name);
	}
	put_token(fp, "\nTYPE");
	put_token(fp, "DISCRETE_LINEAR_DOUBLE");
	put_token(fp, "\nVALUES");
	temp = get_num();
	sprintf(token, "%d\n", temp);
	put_token(fp, token);

	for(i=0; i<temp; i++) {
		sprintf(token, "\n%f", get_time_at_index(i));
		put_token(fp, token);
		sprintf(token, "%f",   get_value_at_index(i));
		put_token(fp, token);
	}

	put_token(fp, "\nINTERP");
	if(get_interpolation()) {
		put_token(fp, "ON");
	} else {
		put_token(fp, "OFF");
	}

	put_token(fp, "\nCLOCK");
	if(!get_clock()) {
		put_token(fp, "NULL");
	} else if(get_clock() == &global_clock) {
		put_token(fp, "DEFAULT");
	} else {
//		put_token(fp, clk->get_name());
fprintf(stderr,"Unable to output nondefault clock name yet.\n");
	}

	put_token(fp, "\nEND_ENVELOPE");

	return(TRUE);

}

