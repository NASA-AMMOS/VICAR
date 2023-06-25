// hazeinfo.C

#include "grape/hazeinfo.h"

char	*haze_model_master_list[NUM_HAZE_MODELS] =
        {
		"HAZE_NONE",
		"HAZE_Z_V1",
		"HAZE_J_V1"
	};

int	haze_model_from_type(char *type)
{
        int     i;

        for(i=0; i<NUM_HAZE_MODELS; i++) {
                if(!strcmp(haze_model_master_list[i], type)) {
                        return(i);
                }
        }
        return(-1);
}


int ZarehHazeInfo::parse_in(Dataport *fp)
{
char      token[4096];

   /*
   ** Loop, get tokens and compare the tokens to key words. This way the
   ** Keword value pair can be specified in any order.
   */
   do 
      {
      if(!get_next_token(fp, token)) 
         {
         fprintf(stderr," Whoops - Unexpected EOF in haze info file\n");
         return(FALSE);
         }

      if(!strcmp(token, "VERSION")) 
         {
         if(!get_next_token(fp, token)) 
            {
            fprintf(stderr," Whoops - Unexpected EOF in haze info file\n");
            return(FALSE);
            }

         if(strcmp(token, "1.0")) 
            {
            fprintf(stderr," Whoops - Unexpected version number encountered in parsing haze info object\n");
            fprintf(stderr,"   Token=>%s<  Expecting >1.0<\n", token);
            return(FALSE);
            }

         version = strdup(token);
	} else if(!strcmp(token, "HAZE_Z_V1")) {
		// do nothing
         } 
      else 
         if(!strcmp(token, "HAZE_COLOR")) 
            {
            if(!haze_red.parse_in(fp)) 
               {
               fprintf(stderr," Whoops - Problem parsing in haze info red value.\n");
               return(FALSE);
               }

            if(!haze_green.parse_in(fp)) 
               {
               fprintf(stderr," Whoops - Problem parsing in haze info green value.\n");
               return(FALSE);
               }

            if(!haze_blue.parse_in(fp)) 
               {
               fprintf(stderr," Whoops - Problem parsing in haze info blue value.\n");
               return(FALSE);
               }
            }
         else
            if(!strcmp(token, "SKY_COLOR")) 
               {
               if(!sky_red.parse_in(fp)) 
                  {
                  fprintf(stderr," Whoops - Problem parsing in sky info red value.\n");
                  return(FALSE);
                  }

               if(!sky_green.parse_in(fp)) 
                  {
                  fprintf(stderr," Whoops - Problem parsing in sky info green value.\n");
                  return(FALSE);
                  }

               if(!sky_blue.parse_in(fp)) 
                  {
                  fprintf(stderr," Whoops - Problem parsing in sky info blue value.\n");
                  return(FALSE);
                  }
               } 
            else
               if(!strcmp(token, "HAZE_MODE")) 
                  {
                  if(!get_next_token(fp, token)) 
                     {
                     fprintf(stderr," Whoops - Problem parsing in haze mode value.\n");
                     return(FALSE);
                     }
                  HazeMode = atoi(token);
                  } 
               else
                  if(!strcmp(token, "HAZE_DIST_EXP")) 
                     {
                     if(!HazeDistExp.parse_in(fp)) 
                        {
                        fprintf(stderr," Whoops - Problem parsing in haze dist exponent value.\n");
                        return(FALSE);
                        }
                     } 
                  else
                     if(!strcmp(token, "HAZE_SYMMETRIC")) 
                        {
            		if(!get_next_token(fp, token)) 
                           {
                           fprintf(stderr," Whoops - Unexpected EOF in scene file\n");
                           return(FALSE);
                           }
                        HazeSymmetric = atoi(token);
                        } 
                     else
                        if(!strcmp(token, "HAZE_SKY_PITCH")) 
                           {
                           if(!HazeSkyPitch.parse_in(fp)) 
                              {
                              fprintf(stderr," Whoops - Problem parsing in haze sky pitch value.\n");
                              return(FALSE);
                              }
                           } 
                        else
                           if(!strcmp(token, "HAZE_GROUND_PITCH")) 
                              {
                              if(!HazeGroundPitch.parse_in(fp)) 
                                 {
                                 fprintf(stderr," Whoops - Problem parsing in haze ground pitch value.\n");
                                 return(FALSE);
                                 }
                              } 
                           else
                              if(!strcmp(token, "HAZE_START_DIST")) 
                                 {
                                 if(!HazeStartDist.parse_in(fp)) 
                                    {
                                    fprintf(stderr," Whoops - Problem parsing in haze start distance value.\n");
                                    return(FALSE);
                                    }
                                 } 
                              else
                                 if(!strcmp(token, "HAZE_END_DIST")) 
                                    {
                                    if(!HazeEndDist.parse_in(fp)) 
                                       {
                                       fprintf(stderr," Whoops - Problem parsing in haze end dist value.\n");
                                       return(FALSE);
                                       }
                                    } 
                                 else
                                    if(!strcmp(token, "HAZE_START_PERC")) 
                                       {
                                       if(!HazeStartPerc.parse_in(fp)) 
                                          {
                                          fprintf(stderr," Whoops - Problem parsing in haze start percentage value.\n");
                                          return(FALSE);
                                          }
                                       } 
                                    else
                                       if(!strcmp(token, "HAZE_END_PERC")) 
                                          {
                                          if(!HazeEndPerc.parse_in(fp)) 
                                             {
                                             fprintf(stderr," Whoops - Problem parsing in haze end percentage value.\n");
                                             return(FALSE);
                                             }
                                          } 
                                       else 
                                          if(strcmp(token, "HAZE_END")) 
                                             {
                                             fprintf(stderr," Whoops - Unexpected token encountered in parsing haze info object\n");
                                             fprintf(stderr,"   Token=>%s<  Expecting >HAZE_END<\n", token);
                                             return(FALSE);
                                             }
      } /* do */ 
   while (strcmp(token, "HAZE_END"));

   return(TRUE);
        
} /* parsein */

int	ZarehHazeInfo::parse_out(Dataport *fp, int expand)
{
	char	token[4096];

	put_token(fp, "\nHAZE_Z_V1");

	put_token(fp, "\nVERSION");
	put_token(fp, "1.0");

	put_token(fp, "\nHAZE_MODE");
	sprintf(token,"%d",HazeMode);
	put_token(fp, token);

	put_token(fp, "\nHAZE_COLOR");
	haze_red.parse_out(fp, expand);
	haze_green.parse_out(fp, expand);
	haze_blue.parse_out(fp, expand);

	put_token(fp, "\nSKY_COLOR");
	sky_red.parse_out(fp, expand);
	sky_green.parse_out(fp, expand);
	sky_blue.parse_out(fp, expand);

	put_token(fp, "\nHAZE_DIST_EXP");
	HazeDistExp.parse_out(fp, expand);
	
	put_token(fp, "\nHAZE_SYMMETRIC");
	sprintf(token,"%d",HazeSymmetric);
	put_token(fp, token);

	put_token(fp, "\nHAZE_SKY_PITCH");
	HazeSkyPitch.parse_out(fp, expand);
	
	put_token(fp, "\nHAZE_GROUND_PITCH");
	HazeGroundPitch.parse_out(fp, expand);
	
	put_token(fp, "\nHAZE_START_DIST");
	HazeStartDist.parse_out(fp, expand);
	
	put_token(fp, "\nHAZE_END_DIST");
	HazeEndDist.parse_out(fp, expand);
	
	put_token(fp, "\nHAZE_START_PERC");
	HazeStartPerc.parse_out(fp, expand);
	
	put_token(fp, "\nHAZE_END_PERC");
	HazeEndPerc.parse_out(fp, expand);
	
	put_token(fp, "\nHAZE_END");
	
	return(TRUE);
}


int	JohnHazeInfo::parse_in(Dataport *fp)
{
	char      token[4096];

   /*
   ** Loop, get tokens and compare the tokens to key words. This way the
   ** Keword value pair can be specified in any order.
   */
   do {
	if(!get_next_token(fp, token)) {
		fprintf(stderr," Whoops - Unexpected EOF in haze info file\n");
		return(FALSE);
	}

	if(!strcmp(token, "VERSION")) {
	    if(!get_next_token(fp, token)) 
            {
            fprintf(stderr," Whoops - Unexpected EOF in haze info file\n");
            return(FALSE);
            }

         if(strcmp(token, "1.0")) 
            {
            fprintf(stderr," Whoops - Unexpected version number encountered in parsing haze info object\n");
            fprintf(stderr,"   Token=>%s<  Expecting >1.0<\n", token);
            return(FALSE);
            }

         version = strdup(token);
	} else if(!strcmp(token, "HAZE_J_V1")) {
		// do nothing
	} else if(!strcmp(token, "HAZE_COLOR")) {
            if(!haze_red.parse_in(fp)) 
               {
               fprintf(stderr," Whoops - Problem parsing in haze info red value.\n");
               return(FALSE);
               }

            if(!haze_green.parse_in(fp)) 
               {
               fprintf(stderr," Whoops - Problem parsing in haze info green value.\n");
               return(FALSE);
               }

            if(!haze_blue.parse_in(fp)) 
               {
               fprintf(stderr," Whoops - Problem parsing in haze info blue value.\n");
               return(FALSE);
               }
	} else if(!strcmp(token, "SKY_COLOR")) {
               if(!sky_red.parse_in(fp)) 
                  {
                  fprintf(stderr," Whoops - Problem parsing in sky info red value.\n");
                  return(FALSE);
                  }

               if(!sky_green.parse_in(fp)) 
                  {
                  fprintf(stderr," Whoops - Problem parsing in sky info green value.\n");
                  return(FALSE);
                  }

               if(!sky_blue.parse_in(fp)) 
                  {
                  fprintf(stderr," Whoops - Problem parsing in sky info blue value.\n");
                  return(FALSE);
                  }
	} else if(!strcmp(token, "HAZE_A")) {
                     if(!Haze_A.parse_in(fp)) 
                        {
                        fprintf(stderr," Whoops - Problem parsing in haze dist exponent value.\n");
                        return(FALSE);
                        }
	} else if(!strcmp(token, "HAZE_B")) {
                           if(!Haze_B.parse_in(fp)) 
                              {
                              fprintf(stderr," Whoops - Problem parsing in haze sky pitch value.\n");
                              return(FALSE);
                              }
	}  else if(strcmp(token, "HAZE_END")) {
                  fprintf(stderr," Whoops - Unexpected token encountered in parsing haze info object\n");
                  fprintf(stderr,"   Token=>%s<  Expecting >HAZE_END<\n", token);
                  return(FALSE);
	}
   }  while (strcmp(token, "HAZE_END"));

   return(TRUE);
}

int	JohnHazeInfo::parse_out(Dataport *fp, int expand)
{
	put_token(fp, "\nHAZE_J_V1");

	put_token(fp, "\nVERSION");
	put_token(fp, "1.0");

	put_token(fp, "\nHAZE_COLOR");
	haze_red.parse_out(fp, expand);
	haze_green.parse_out(fp, expand);
	haze_blue.parse_out(fp, expand);

	put_token(fp, "\nSKY_COLOR");
	sky_red.parse_out(fp, expand);
	sky_green.parse_out(fp, expand);
	sky_blue.parse_out(fp, expand);

	put_token(fp, "\nHAZE_A");
	Haze_A.parse_out(fp, expand);
	
	put_token(fp, "\nHAZE_B");
	Haze_B.parse_out(fp, expand);
	
	put_token(fp, "\nHAZE_END");
	
	return(TRUE);
}


HazeInfo *create_haze_info(Dataport *fp)
{
	char	token[4096];
	HazeInfo	*hzin;

	get_next_token(fp, token);
	switch(haze_model_from_type(token)) {
		case ZAREH_HAZE :
			hzin = new ZarehHazeInfo();
			break;
		case JOHN_HAZE :
			hzin = new JohnHazeInfo();
			break;
		default:
			fprintf(stderr,"Whoops - Unrecognized haze model = %s\n", token);
			hzin = NULL;
			break;
	}
	return(hzin);
}


