// write .lbl header tables for PDS-4
// needed info:
// OBJ Material Reference Table offset
// OBJ int count_vertices 
// bool write_out_normals

#include <stdio.h>
#include <stdlib.h>

void write_label_pds(FILE *output_lbl_file) {
    
    int lbl_offset = 0;
    int count_vertices = 0;
    int count_normals = 0;
    int counter_texture_coords = 0;
    bool write_out_normals = false;

    fprintf(output_lbl_file, "<Table_Delimited>\n");
    fprintf(output_lbl_file, "\t<name>OBJ Material Reference Table</name>\n");
    fprintf(output_lbl_file, "\t<offset unit=\"byte\">%u</offset>\n",
            lbl_offset);
    fprintf(output_lbl_file, "\t<parsing_standard_id>PDS DSV 1</parsing_standard_id>\n");
    fprintf(output_lbl_file, "\t<description>Material file for the OBJ file</description>\n");
    fprintf(output_lbl_file, "\t<records>1</records>\n");
    fprintf(output_lbl_file, "\t<record_delimiter>Carriage-Return Line-Feed</record_delimiter>\n");
    fprintf(output_lbl_file, "\t<field_delimiter>Horizontal Tab</field_delimiter>\n");
    fprintf(output_lbl_file, "\t<Record_Delimited>\n");
    fprintf(output_lbl_file, "\t\t<fields>1</fields>\n");
    fprintf(output_lbl_file, "\t\t<groups>0</groups>\n");
    fprintf(output_lbl_file, "\t\t<Field_Delimited>\n");
    fprintf(output_lbl_file, "\t\t\t<name>mtllib</name>\n");
    fprintf(output_lbl_file, "\t\t\t<field_number>1</field_number>\n");
    fprintf(output_lbl_file, "\t\t\t<data_type>ASCII_String</data_type>\n");
    fprintf(output_lbl_file, "\t\t\t<description>Description of .mtl file.</description>\n");
    fprintf(output_lbl_file, "\t\t</Field_Delimited>\n");
    fprintf(output_lbl_file, "\t</Record_Delimited>\n");
    fprintf(output_lbl_file, "</Table_Delimited>\n");

    //Write corresponding material name into the output file
    // Get the current position in the output file for PDS-4 label
    //lbl_offset = ftell(tfile);
    fprintf(output_lbl_file, "<Table_Delimited>\n");
    fprintf(output_lbl_file, "\t<name>OBJ Material Name Table</name>\n");
    fprintf(output_lbl_file, "\t<offset unit=\"byte\">%u</offset>\n",
            lbl_offset);
    fprintf(output_lbl_file, "\t<parsing_standard_id>PDS DSV 1</parsing_standard_id>\n");
    fprintf(output_lbl_file, "\t<description>Material name referenced by OBJ file</description>\n");
    fprintf(output_lbl_file, "\t<records>1</records>\n");
    fprintf(output_lbl_file, "\t<record_delimiter>Carriage-Return Line-Feed</record_delimiter>\n");
    fprintf(output_lbl_file, "\t<field_delimiter>Horizontal Tab</field_delimiter>\n");
    fprintf(output_lbl_file, "\t<Record_Delimited>\n");
    fprintf(output_lbl_file, "\t\t<fields>1</fields>\n");
    fprintf(output_lbl_file, "\t\t<groups>0</groups>\n");
    fprintf(output_lbl_file, "\t\t<Field_Delimited>\n");
    fprintf(output_lbl_file, "\t\t\t<name>usemtl</name>\n");
    fprintf(output_lbl_file, "\t\t\t<field_number>1</field_number>\n");
    fprintf(output_lbl_file, "\t\t\t<data_type>ASCII_String</data_type>\n");
    fprintf(output_lbl_file, "\t\t\t<description>Describes which material to use.</description>\n");
    fprintf(output_lbl_file, "\t\t</Field_Delimited>\n");
    fprintf(output_lbl_file, "\t</Record_Delimited>\n");
    fprintf(output_lbl_file, "</Table_Delimited>\n");


    // only write out PDS tables if there is data
    if (count_vertices > 0) {
        // write out PDS-4 Label Fragement for Vertices Table
        fprintf (output_lbl_file, "<Table_Delimited>\n");
        fprintf (output_lbl_file, "\t<name>OBJ Vertices Table</name>\n");
        fprintf (output_lbl_file, "\t<offset unit=\"byte\">%u</offset>\n",
                lbl_offset);
        fprintf(output_lbl_file,"\t<parsing_standard_id>PDS DSV 1</parsing_standard_id>\n");
        fprintf(output_lbl_file,"\t<description>Table of Vertices of the OBJ file</description>\n");
        fprintf(output_lbl_file, "\t<records>%d</records>\n",count_vertices);
        fprintf(output_lbl_file,"\t<record_delimiter>Carriage-Return Line-Feed</record_delimiter>\n");
        fprintf(output_lbl_file,"\t<field_delimiter>Horizontal Tab</field_delimiter>\n");
        fprintf(output_lbl_file,"\t<Record_Delimited>\n");
        fprintf(output_lbl_file,"\t\t<fields>4</fields>\n");
        fprintf(output_lbl_file,"\t\t<groups>0</groups>\n");

        fprintf(output_lbl_file,"\t\t<Field_Delimited>\n");
        fprintf(output_lbl_file,"\t\t\t<name>OBJ Datatype</name>\n");
        fprintf(output_lbl_file,"\t\t\t<field_number>1</field_number>\n");
        fprintf(output_lbl_file,"\t\t\t<data_type>ASCII_String</data_type>\n");
        fprintf(output_lbl_file,"\t\t\t<description>v letter for vertex</description>\n");
        fprintf(output_lbl_file,"\t\t</Field_Delimited>\n");

        fprintf(output_lbl_file,"\t\t<Field_Delimited>\n");
        fprintf(output_lbl_file,"\t\t\t<name>VertX</name>\n");
        fprintf(output_lbl_file,"\t\t\t<field_number>2</field_number>\n");
        fprintf(output_lbl_file,"\t\t\t<data_type>ASCII_Real</data_type>\n");
        fprintf(output_lbl_file,"\t\t\t<description>X coordinate of the Vertex in Specific Reference Frame.</description>\n");
        fprintf(output_lbl_file,"\t\t</Field_Delimited>\n");

        fprintf(output_lbl_file,"\t\t<Field_Delimited>\n");
        fprintf(output_lbl_file,"\t\t\t<name>Vertex Y</name>\n");
        fprintf(output_lbl_file,"\t\t\t<field_number>3</field_number>\n");
        fprintf(output_lbl_file,"\t\t\t<data_type>ASCII_Real</data_type>\n");
        fprintf(output_lbl_file,"\t\t\t<description>Y coordinate of the vertex in Specific Site Reference Frame.</description>\n");
        fprintf(output_lbl_file,"\t\t</Field_Delimited>\n");

        fprintf(output_lbl_file,"\t\t<Field_Delimited>\n");
        fprintf(output_lbl_file,"\t\t\t<name>Vertex Z</name>\n");
        fprintf(output_lbl_file,"\t\t\t<field_number>4</field_number>\n");
        fprintf(output_lbl_file,"\t\t\t<data_type>ASCII_Real</data_type>\n");
        fprintf(output_lbl_file,"\t\t\t<description>Z coordinate of the vertex in Specific Reference Frame.</description>\n");
        fprintf(output_lbl_file,"\t\t</Field_Delimited>\n");

        fprintf(output_lbl_file,"\t</Record_Delimited>\n");
        fprintf(output_lbl_file,"</Table_Delimited>\n");
    }

    // the only table that is optional
    if ((count_vertices > 0) && write_out_normals) {
    } // end of write normals


    if (true) {
        // write out PDS-4 Label Fragement for normals Table
        fprintf (output_lbl_file, "<Table_Delimited>\n");
        fprintf (output_lbl_file, "\t<name>OBJ Normals Table</name>\n");
        fprintf (output_lbl_file, "\t<offset unit=\"byte\">%u</offset>\n", lbl_offset);
        fprintf(output_lbl_file,"\t<parsing_standard_id>PDS DSV 1</parsing_standard_id>\n");
        fprintf(output_lbl_file,"\t<description>Table of Normals of the OBJ file</description>\n");
        fprintf(output_lbl_file, "\t<records>%d</records>\n",count_normals);
        fprintf(output_lbl_file,"\t<record_delimiter>Carriage-Return Line-Feed</record_delimiter>\n");
        fprintf(output_lbl_file,"\t<field_delimiter>Horizontal Tab</field_delimiter>\n");
        fprintf(output_lbl_file,"\t<Record_Delimited>\n");
        fprintf(output_lbl_file,"\t\t<fields>4</fields>\n");
        fprintf(output_lbl_file,"\t\t<groups>0</groups>\n");

        fprintf(output_lbl_file,"\t\t<Field_Delimited>\n");
        fprintf(output_lbl_file,"\t\t\t<name>OBJ Datatype</name>\n");
        fprintf(output_lbl_file,"\t\t\t<field_number>1</field_number>\n");
        fprintf(output_lbl_file,"\t\t\t<data_type>ASCII_String</data_type>\n");
        fprintf(output_lbl_file,"\t\t\t<description>vn letter for normal</description>\n");
        fprintf(output_lbl_file,"\t\t</Field_Delimited>\n");

        fprintf(output_lbl_file,"\t\t<Field_Delimited>\n");
        fprintf(output_lbl_file,"\t\t\t<name>Normal component Vx</name>\n");
        fprintf(output_lbl_file,"\t\t\t<field_number>2</field_number>\n");
        fprintf(output_lbl_file,"\t\t\t<data_type>ASCII_Real</data_type>\n");
        fprintf(output_lbl_file,"\t\t\t<description>Vx coordinate of the Normal in Specific Reference Frame.</description>\n");
        fprintf(output_lbl_file,"\t\t</Field_Delimited>\n");

        fprintf(output_lbl_file,"\t\t<Field_Delimited>\n");
        fprintf(output_lbl_file,"\t\t\t<name>Normal component Vy</name>\n");
        fprintf(output_lbl_file,"\t\t\t<field_number>3</field_number>\n");
        fprintf(output_lbl_file,"\t\t\t<data_type>ASCII_Real</data_type>\n");
        fprintf(output_lbl_file,"\t\t\t<description>Vy coordinate of the normal in Specific Reference Frame.</description>\n");
        fprintf(output_lbl_file,"\t\t</Field_Delimited>\n");

        fprintf(output_lbl_file,"\t\t<Field_Delimited>\n");
        fprintf(output_lbl_file,"\t\t\t<name>Normal component Vz</name>\n");
        fprintf(output_lbl_file,"\t\t\t<field_number>4</field_number>\n");
        fprintf(output_lbl_file,"\t\t\t<data_type>ASCII_Real</data_type>\n");
        fprintf(output_lbl_file,"\t\t\t<description>Vz coordinate of the normal in Specific Reference Frame.</description>\n");
        fprintf(output_lbl_file,"\t\t</Field_Delimited>\n");

        fprintf(output_lbl_file,"\t</Record_Delimited>\n");
        fprintf(output_lbl_file,"</Table_Delimited>\n");
    }

    //write texture coordinates
    // Get the current position in the output file for PDS-4 label
    //lbl_offset = ftell(tfile);
    //unsigned long counter_texture_coords = 0;
    //counter_texture_coords +=3;

    //} //end of write texture coords

    // only write out PDS tables if there is data
    if (count_vertices > 0) {
    //write out PDS-4 Label Fragment for Texture Table
        fprintf (output_lbl_file, "<Table_Delimited>\n");
        fprintf (output_lbl_file, "\t<name>OBJ Texture Coordinates Table</name>\n");
        fprintf (output_lbl_file, "\t<offset unit=\"byte\">%u</offset>\n", lbl_offset);
        fprintf (output_lbl_file, "\t<parsing_standard_id>PDS DSV 1</parsing_standard_id>\n");
        fprintf (output_lbl_file, "\t<description>Table of Texture Coordinates of the OBJ file</description>\n");
        fprintf (output_lbl_file, "\t<records>%d</records>\n", counter_texture_coords);
        fprintf (output_lbl_file, "\t<record_delimiter>Carriage-Return Line-Feed</record_delimiter>\n");
        fprintf (output_lbl_file, "\t<field_delimiter>Horizontal Tab</field_delimiter>\n");
        fprintf (output_lbl_file, "\t<Record_Delimited>\n");
        fprintf (output_lbl_file, "\t\t<fields>3</fields>\n");
        fprintf (output_lbl_file, "\t\t<groups>0</groups>\n");
        fprintf (output_lbl_file, "\t\t<Field_Delimited>\n");
        fprintf (output_lbl_file, "\t\t\t<name>OBJ Datatype</name>\n");
        fprintf (output_lbl_file, "\t\t\t<field_number>1</field_number>\n");
        fprintf (output_lbl_file, "\t\t\t<data_type>ASCII_String</data_type>\n");
        fprintf (output_lbl_file, "\t\t\t<description>vt letter for UV Texture Mapping</description>\n");
        fprintf (output_lbl_file, "\t\t</Field_Delimited>\n");
        fprintf (output_lbl_file, "\t\t<Field_Delimited>\n");
        fprintf (output_lbl_file, "\t\t\t<name>TextU</name>\n");
        fprintf (output_lbl_file, "\t\t\t<field_number>2</field_number>\n");
        fprintf (output_lbl_file, "\t\t\t<data_type>ASCII_Real</data_type>\n");
        fprintf (output_lbl_file, "\t\t\t<description>U coordinate of the UV Texture Mapping.</description>\n");
        fprintf (output_lbl_file, "\t\t</Field_Delimited>\n");
        fprintf (output_lbl_file, "\t\t<Field_Delimited>\n");
        fprintf (output_lbl_file, "\t\t\t<name>Texture Mapping Coordinate V</name>\n");
        fprintf (output_lbl_file, "\t\t\t<field_number>3</field_number>\n");
        fprintf (output_lbl_file, "\t\t\t<data_type>ASCII_Real</data_type>\n");
        fprintf (output_lbl_file, "\t\t\t<description>V Coordinate of the UV Texture Mapping.</description>\n");
        fprintf (output_lbl_file, "\t\t</Field_Delimited>\n");
        fprintf (output_lbl_file, "\t</Record_Delimited>\n");
        fprintf (output_lbl_file, "</Table_Delimited>\n");
    }
    //write faces
    // Get the current position in the output file for PDS-4 label
    //lbl_offset = ftell(tfile);
    unsigned long tc = 0;
    //!!!! pass this as an array
    if (write_out_normals) {
    } else {
      }

    //} //end of write out faces

    // only write out PDS tables if there is data
    if (count_vertices > 0) {
        //write out PDS-4 Label Fragment for Faces Table
        fprintf(output_lbl_file, "<Table_Delimited>\n");
        fprintf(output_lbl_file, "\t<name>Faces Table</name>\n");
        fprintf(output_lbl_file, "\t<offset unit=\"byte\">%u</offset>\n", lbl_offset);
        fprintf(output_lbl_file, "\t<parsing_standard_id>PDS DSV 1</parsing_standard_id>\t");
        fprintf(output_lbl_file, "\t<description>Table of Faces of the OBJ file</description>\n");
        fprintf(output_lbl_file, "\t<records>%ld</records>\n", (tc-1)/3);
        fprintf(output_lbl_file, "\t<record_delimiter>Carriage-Return Line-Feed</record_delimiter>\n");
        fprintf(output_lbl_file, "\t<field_delimiter>Horizontal Tab</field_delimiter>\n");
        fprintf(output_lbl_file, "\t<Record_Delimited>\n");
        fprintf(output_lbl_file, "\t\t<fields>4</fields>\n");
        fprintf(output_lbl_file, "\t\t<groups>0</groups>\n");
        fprintf(output_lbl_file, "\t\t<Field_Delimited>\n");
        fprintf(output_lbl_file, "\t\t\t<name>OBJ Datatype</name>\n");
        fprintf(output_lbl_file, "\t\t\t<field_number>1</field_number>\n");
        fprintf(output_lbl_file, "\t\t\t<data_type>ASCII_String</data_type>\n");
        fprintf(output_lbl_file, "\t\t\t<description>Face Element</description>\n");
        fprintf(output_lbl_file, "\t\t</Field_Delimited>\n");
        fprintf(output_lbl_file, "\t\t<Field_Delimited>\n");
        fprintf(output_lbl_file, "\t\t\t<name>Index 1</name>\n");
        fprintf(output_lbl_file, "\t\t\t<field_number>2</field_number>\n");
        fprintf(output_lbl_file, "\t\t\t<data_type>ASCII_String</data_type>\n");
        fprintf(output_lbl_file, "\t\t\t<description>Index of the first vertex of the face.</description>\n");
        fprintf(output_lbl_file, "\t\t</Field_Delimited>\n");
        fprintf(output_lbl_file, "\t\t<Field_Delimited>\n");
        fprintf(output_lbl_file, "\t\t\t<name>Index 2</name>\n");
        fprintf(output_lbl_file, "\t\t\t<field_number>3</field_number>\n");
        fprintf(output_lbl_file, "\t\t\t<data_type>ASCII_String</data_type>\n");
        fprintf(output_lbl_file, "\t\t\t<description>Index of the second vertex of the face.</description>\n");
        fprintf(output_lbl_file, "\t\t\t</Field_Delimited>\n");
        fprintf(output_lbl_file, "\t\t<Field_Delimited>\n");
        fprintf(output_lbl_file, "\t\t\t<name>Index 3</name>\n");
        fprintf(output_lbl_file, "\t\t\t<field_number>4</field_number>\n");
        fprintf(output_lbl_file, "\t\t\t<data_type>ASCII_String</data_type>\n");
        fprintf(output_lbl_file, "\t\t\t<description>Index of the third vertex of the face.</description>\n");
        fprintf(output_lbl_file, "\t\t</Field_Delimited>\n");
        fprintf(output_lbl_file, "\t</Record_Delimited>\n");
        fprintf(output_lbl_file, "</Table_Delimited>\n");
    }
    //close the label file now
    fclose(output_lbl_file);
}
