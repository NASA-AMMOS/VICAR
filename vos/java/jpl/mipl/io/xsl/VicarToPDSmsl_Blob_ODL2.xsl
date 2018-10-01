<?xml version='1.0' encoding='UTF-8' ?>
<xsl:stylesheet version="2.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<!-- Author: Hyun Hee Lee, Steve Levoe 
MSL version july 2010 JPL/MIPL - Steve Levoe
MSL chemCAM BLOB version feb 2011 _ Steve Levoe -->
<xsl:output method="xml" indent="yes" encoding="utf-8"/>
	
	<xsl:template match="/">
		<xsl:apply-templates/>
	</xsl:template>
	
	<xsl:template match="VICAR_LABEL">
	<PDS_LABEL>	
		<xsl:apply-templates select="SYSTEM"/>				
		<xsl:apply-templates select="CLASS"/>
		<xsl:apply-templates select="PROPERTY[@name='IDENTIFICATION']"/>
		<xsl:apply-templates select="PROPERTY[@name='TELEMETRY']"/>
		<xsl:apply-templates select="PROPERTY[@name='PDS_HISTORY']"/>
		<xsl:apply-templates select="PROPERTY[@name='GEOMETRIC_CAMERA_MODEL']"/>
		<xsl:apply-templates select="PROPERTY[@name='ROVER_COORDINATE_SYSTEM']"/>	
		<xsl:apply-templates select="PROPERTY[@name='SITE_COORDINATE_SYSTEM']"/>	
		<xsl:apply-templates select="PROPERTY[@name='LOCAL_LEVEL_COORDINATE_SYSTEM']"/>		
		<xsl:apply-templates select="PROPERTY[@name='RSM_COORDINATE_SYSTEM']"/>
		<xsl:apply-templates select="PROPERTY[@name='ARM_COORDINATE_SYSTEM']"/>
		<xsl:apply-templates select="PROPERTY[@name='RSM_ARTICULATION_STATE']"/>
		<xsl:apply-templates select="PROPERTY[@name='ARM_ARTICULATION_STATE']"/>
		<xsl:apply-templates select="PROPERTY[@name='CHASSIS_ARTICULATION_STATE']"/>
		<xsl:apply-templates select="PROPERTY[@name='HGA_ARTICULATION_STATE']"/>
		<xsl:apply-templates select="PROPERTY[@name='OBSERVATION_REQUEST_PARMS']"/>	
		<xsl:apply-templates select="PROPERTY[@name='IMAGE_REQUEST_PARMS']"/>
		<xsl:apply-templates select="PROPERTY[@name='SUBFRAME_REQUEST_PARMS']"/>
		<xsl:apply-templates select="PROPERTY[@name='THUMBNAIL_REQUEST_PARMS']"/>
		<xsl:apply-templates select="PROPERTY[@name='ROW_SUMMATION_REQUEST_PARMS']"/>
		<xsl:apply-templates select="PROPERTY[@name='COLUMN_SUM_REQUEST_PARMS']"/>
		<xsl:apply-templates select="PROPERTY[@name='REFERENCE_PIXEL_REQUEST_PARMS']"/>
		<xsl:apply-templates select="PROPERTY[@name='HISTOGRAM_REQUEST_PARMS']"/>
		<xsl:apply-templates select="PROPERTY[@name='CHEMCAM_REQUEST_PARMS']"/>
		<xsl:apply-templates select="PROPERTY[@name='INSTRUMENT_STATE_PARMS']"/>
		<xsl:apply-templates select="PROPERTY[@name='COMPRESSION_PARMS']"/>
		<xsl:apply-templates select="PROPERTY[@name='ROVER_DERIVED_GEOMETRY_PARMS']"/>
		<xsl:apply-templates select="PROPERTY[@name='SITE_DERIVED_GEOMETRY_PARMS']"/>
		<xsl:apply-templates select="PROPERTY[@name='DERIVED_IMAGE_PARMS']"/>
		<xsl:apply-templates select="PROPERTY[@name='SURFACE_PROJECTION_PARMS']"/>
		<xsl:apply-templates select="PROPERTY[@name='SURFACE_MODEL_PARMS']"/>
		<!-- CHEMCAM specific to handle binary label data -->
		<xsl:apply-templates select="PROPERTY[@name='CHEM_REQUEST']"/>
		<xsl:apply-templates select="PROPERTY[@name='ANCILLARY_TABLE']"/>
		<xsl:apply-templates select="PROPERTY[@name='SOH_BEFORE_TABLE']"/>
		<xsl:apply-templates select="PROPERTY[@name='SOH_AFTER_TABLE']"/>
		<xsl:apply-templates select="PROPERTY[@name='AUTOFOCUS_TABLE']"/>
		<xsl:apply-templates select="PROPERTY[@name='IMAGE_REPLY_TABLE']"/>
		<xsl:apply-templates select="PROPERTY[@name='IMAGE_HEADER_FOOTER_TABLE']"/>
		<!-- end of CHEMCAM specific to handle binary label data -->
		<xsl:apply-templates select="PROPERTY[@name='TABLE']"/>
		<xsl:apply-templates select="PROPERTY[@name='IMAGE_HEADER']"/>
		<xsl:apply-templates select="PROPERTY[@name='IMAGE_DATA']"/>
		
		<xsl:apply-templates select="PROPERTYS"/>		
		<xsl:apply-templates select="GROUP"/>
	
	</PDS_LABEL>
	</xsl:template>
	

	<xsl:template match="SYSTEM">
		<!-- DO NOTHING IF SYSTEM IS MATCHED -->
		<VICAR_SYSTEM>
			<xsl:apply-templates select="SYSTEM"/>
			<xsl:apply-templates/>
		</VICAR_SYSTEM>	
	</xsl:template>	
	
	<xsl:template match="PROPERTY[@name='IDENTIFICATION']">
		<CLASS>        
		<xsl:attribute name="name"><xsl:value-of select="@name"/></xsl:attribute>		
		<comment>/* IDENTIFICATION DATA ELEMENTS */</comment>	
			<xsl:apply-templates/>
		</CLASS>
	</xsl:template>
	
	<xsl:template match="PROPERTY[@name='TELEMETRY']">
		<CLASS>        
		<xsl:attribute name="name"><xsl:value-of select="@name"/></xsl:attribute>
		<comment>/* TELEMETRY DATA ELEMENTS */</comment>
			<xsl:apply-templates/>
		</CLASS>
	</xsl:template>

	<xsl:template match="PROPERTY[@name='PDS_HISTORY']">
		<CLASS>      
		<xsl:attribute name="name"><xsl:value-of select="@name"/></xsl:attribute>
		<comment>/* HISTORY DATA ELEMENTS */</comment>
		 <GROUP>
		    <xsl:attribute name="name"><xsl:value-of select="@name"/></xsl:attribute>
			<xsl:apply-templates/>
		 </GROUP>
		</CLASS>
	</xsl:template>
	
	<xsl:template match="PROPERTY[@name='GEOMETRIC_CAMERA_MODEL']">
        <comment>/* CAMERA_MODEL DATA ELEMENTS */</comment>
        <GROUP>
		<xsl:attribute name="name"><xsl:value-of select="@name"/></xsl:attribute>
			<xsl:apply-templates/>
		</GROUP>
	</xsl:template>
	
	<xsl:template match="PROPERTY[@name='ROVER_COORDINATE_SYSTEM']">
        <comment>/* COORDINATE SYSTEM STATE: ROVER */</comment>
        <GROUP>
		<xsl:attribute name="name"><xsl:value-of select="@name"/></xsl:attribute>
			<xsl:apply-templates/>
		</GROUP>
	</xsl:template>
		
	<xsl:template match="PROPERTY[@name='LOCAL_LEVEL_COORDINATE_SYSTEM']">
        <comment>/* COORDINATE SYSTEM STATE: LOCAL LEVEL */</comment>
        <GROUP>
		<xsl:attribute name="name"><xsl:value-of select="@name"/></xsl:attribute>
			<xsl:apply-templates/>
		</GROUP>
	</xsl:template>
	
	<xsl:template match="PROPERTY[@name='SITE_COORDINATE_SYSTEM']">
        <comment>/* COORDINATE SYSTEM STATE: SITE */</comment>
        <GROUP>
		<xsl:attribute name="name"><xsl:value-of select="@name"/></xsl:attribute>
			<xsl:apply-templates/>
		</GROUP>
	</xsl:template>
	
	<xsl:template match="PROPERTY[@name='RSM_COORDINATE_SYSTEM']">
        <comment>/* COORDINATE SYSTEM STATE: REMOTE SENSING MAST */</comment>
        <GROUP>
		<xsl:attribute name="name"><xsl:value-of select="@name"/></xsl:attribute>
			<xsl:apply-templates/>
		</GROUP>
	</xsl:template>	
	<xsl:template match="PROPERTY[@name='ARM_COORDINATE_SYSTEM']">
        <comment>/* COORDINATE SYSTEM STATE: ROBOTIC ARM */</comment>
        <GROUP>
		<xsl:attribute name="name"><xsl:value-of select="@name"/></xsl:attribute>
			<xsl:apply-templates/>
		</GROUP>
	</xsl:template>	
		
	<xsl:template match="PROPERTY[@name='RSM_ARTICULATION_STATE']">
        <comment>/* ARTICULATION DEVICE STATE: REMOTE SENSING MAST */</comment>
        <GROUP>
		<xsl:attribute name="name"><xsl:value-of select="@name"/></xsl:attribute>
			<xsl:apply-templates/>
		</GROUP>
	</xsl:template>	
	<xsl:template match="PROPERTY[@name='ARM_ARTICULATION_STATE']">
        <comment>/* ARTICULATION DEVICE STATE: ROBOTIC ARM */</comment>
        <GROUP>
		<xsl:attribute name="name"><xsl:value-of select="@name"/></xsl:attribute>
			<xsl:apply-templates/>
		</GROUP>
	</xsl:template>	
	<xsl:template match="PROPERTY[@name='CHASSIS_ARTICULATION_STATE']">
        <comment>/* ARTICULATION DEVICE STATE: MOBILITY CHASSIS */</comment>
        <GROUP>
		<xsl:attribute name="name"><xsl:value-of select="@name"/></xsl:attribute>
			<xsl:apply-templates/>
		</GROUP>
	</xsl:template>
	<xsl:template match="PROPERTY[@name='HGA_ARTICULATION_STATE']">
        <comment>/* ARTICULATION DEVICE STATE: HIGH GAIN ANTENNA */</comment>
        <GROUP>
		<xsl:attribute name="name"><xsl:value-of select="@name"/></xsl:attribute>
			<xsl:apply-templates/>
		</GROUP>
	</xsl:template>
	<xsl:template match="PROPERTY[@name='OBSERVATION_REQUEST_PARMS']">
        <comment>/* OBSERVATION REQUEST */</comment>
        <GROUP>
		<xsl:attribute name="name"><xsl:value-of select="@name"/></xsl:attribute>
			<xsl:apply-templates/>
		</GROUP>
	</xsl:template>
	<xsl:template match="PROPERTY[@name='IMAGE_REQUEST_PARMS']">
        <comment>/* IMAGE REQUEST */</comment>
        <GROUP>
		<xsl:attribute name="name"><xsl:value-of select="@name"/></xsl:attribute>
			<xsl:apply-templates/>
		</GROUP>
	</xsl:template>
	<xsl:template match="PROPERTY[@name='SUBFRAME_REQUEST_PARMS']">
        <comment>/* SUBFRAME REQUEST */</comment>
        <GROUP>
		<xsl:attribute name="name"><xsl:value-of select="@name"/></xsl:attribute>
			<xsl:apply-templates/>
		</GROUP>
	</xsl:template>
	<xsl:template match="PROPERTY[@name='THUMBNAIL_REQUEST_PARMS']">
        <comment>/* THUMBNAIL REQUEST */</comment>
        <GROUP>
		<xsl:attribute name="name"><xsl:value-of select="@name"/></xsl:attribute>
			<xsl:apply-templates/>
		</GROUP>
	</xsl:template>
	<xsl:template match="PROPERTY[@name='ROW_SUM_REQUEST_PARMS']">
        <comment>/* ROW SUMMATION REQUEST */</comment>
        <GROUP>
		<xsl:attribute name="name"><xsl:value-of select="@name"/></xsl:attribute>
			<xsl:apply-templates/>
		</GROUP>
	</xsl:template>	
	<xsl:template match="PROPERTY[@name='COLUMN_SUM_REQUEST_PARMS']">
        <comment>/* COLUMN SUMMATION REQUEST */</comment>
        <GROUP>
		<xsl:attribute name="name"><xsl:value-of select="@name"/></xsl:attribute>
			<xsl:apply-templates/>
		</GROUP>
	</xsl:template>
	<xsl:template match="PROPERTY[@name='REFERENCE_PIXEL_REQUEST_PARMS']">
        <comment>/* REFERENCE PIXEL REQUEST */</comment>
        <GROUP>
		<xsl:attribute name="name"><xsl:value-of select="@name"/></xsl:attribute>
			<xsl:apply-templates/>
		</GROUP>
	</xsl:template>
	<xsl:template match="PROPERTY[@name='HISTOGRAM_REQUEST_PARMS']">
        <comment>/* HISTOGRAM REQUEST */</comment>
        <GROUP>
		<xsl:attribute name="name"><xsl:value-of select="@name"/></xsl:attribute>
			<xsl:apply-templates/>
		</GROUP>
	</xsl:template>
	<xsl:template match="PROPERTY[@name='CHEMCAM_REQUEST_PARMS']">
        <comment>/* CHEMCAM REQUEST */</comment>
        <GROUP>
		<xsl:attribute name="name"><xsl:value-of select="@name"/></xsl:attribute>
			<xsl:apply-templates/>
		</GROUP>
	</xsl:template>
	<xsl:template match="PROPERTY[@name='INSTRUMENT_STATE_PARMS']">
        <comment>/* INSTRUMENT STATE RESULTS */</comment>
        <GROUP>
		<xsl:attribute name="name"><xsl:value-of select="@name"/></xsl:attribute>
			<xsl:apply-templates/>
		</GROUP>
	</xsl:template>
	<xsl:template match="PROPERTY[@name='COMPRESSION_PARMS']">
        <comment>/* COMPRESSION RESULTS */</comment>
        <GROUP>
		<xsl:attribute name="name"><xsl:value-of select="@name"/></xsl:attribute>
			<xsl:apply-templates/>
		</GROUP>
	</xsl:template>
	
	<xsl:template match="PROPERTY[@name='ROVER_DERIVED_GEOMETRY_PARMS']">
        <comment>/* DERIVED GEOMETRY DATA ELEMENTS: ROVER FRAME */</comment>
        <GROUP>
		<xsl:attribute name="name"><xsl:value-of select="@name"/></xsl:attribute>
			<xsl:apply-templates/>
		</GROUP>
	</xsl:template>
	<xsl:template match="PROPERTY[@name='SITE_DERIVED_GEOMETRY_PARMS']">
        <comment>/* DERIVED GEOMETRY DATA ELEMENTS: SITE FRAME */</comment>
        <GROUP>
		<xsl:attribute name="name"><xsl:value-of select="@name"/></xsl:attribute>
			<xsl:apply-templates/>
		</GROUP>
	</xsl:template>
	<xsl:template match="PROPERTY[@name='DERIVED_IMAGE_PARMS']">
        <comment>/* DERIVED IMAGE DATA ELEMENTS */</comment>
        <GROUP>
		<xsl:attribute name="name"><xsl:value-of select="@name"/></xsl:attribute>
			<xsl:apply-templates/>
		</GROUP>
	</xsl:template>
	
	<xsl:template match="PROPERTY[@name='SURFACE_PROJECTION_PARMS']">
        <comment>/* SURFACE PROJECTION DATA ELEMENTS */</comment>
        <GROUP>
		<xsl:attribute name="name"><xsl:value-of select="@name"/></xsl:attribute>
			<xsl:apply-templates/>
		</GROUP>
	</xsl:template>
	
	<xsl:template match="PROPERTY[@name='SURFACE_MODEL_PARMS']">		
        <comment>/* SURFACE MODEL DATA ELEMENTS */</comment>
        <GROUP>
		<xsl:attribute name="name"><xsl:value-of select="@name"/></xsl:attribute>
			<xsl:apply-templates/>
		</GROUP>
	</xsl:template>
	
	<xsl:template match="PROPERTY[@name='CHEM_REQUEST']">		
        <comment>/* CHEMCAM REQUEST PARAMETERS */</comment>
        <GROUP>
		<xsl:attribute name="name"><xsl:value-of select="@name"/></xsl:attribute>
			<xsl:apply-templates/>
		</GROUP>
	</xsl:template>
	
	<xsl:template match="PROPERTY[@name='ANCILLARY_TABLE']">		
        <comment>/* CHEMCAM ANCILLARY TABLE PARAMETERS */</comment>
        <OBJECT>
		<xsl:attribute name="name"><xsl:value-of select="@name"/></xsl:attribute>
			<xsl:apply-templates/>
		</OBJECT>
	</xsl:template>
	
	<xsl:template match="PROPERTY[@name='SOH_BEFORE_TABLE']">		
        <comment>/* CHEMCAM SOH BEFORE TABLE PARAMETERS */</comment>
        <OBJECT>
		<xsl:attribute name="name"><xsl:value-of select="@name"/></xsl:attribute>
			<xsl:apply-templates/>
		</OBJECT>
	</xsl:template>
	
	<xsl:template match="PROPERTY[@name='SOH_AFTER_TABLE']">		
        <comment>/* CHEMCAM SOH AFTER TABLE PARAMETERS */</comment>
        <OBJECT>
		<xsl:attribute name="name"><xsl:value-of select="@name"/></xsl:attribute>
			<xsl:apply-templates/>
		</OBJECT>
	</xsl:template>
	
	<xsl:template match="PROPERTY[@name='AUTOFOCUS_TABLE']">		
        <comment>/* CHEMCAM AUTOFOCUS TABLE PARAMETERS */</comment>
        <OBJECT>
		<xsl:attribute name="name"><xsl:value-of select="@name"/></xsl:attribute>
			<xsl:apply-templates/>
		</OBJECT>
	</xsl:template>
	
	<xsl:template match="PROPERTY[@name='IMAGE_REPLY_TABLE']">		
        <comment>/* CHEMCAM IMAGE REPLY TABLE PARAMETERS */</comment>
        <OBJECT>
		<xsl:attribute name="name"><xsl:value-of select="@name"/></xsl:attribute>
			<xsl:apply-templates/>
		</OBJECT>
	</xsl:template>
	
	<xsl:template match="PROPERTY[@name='IMAGE_HEADER_FOOTER_TABLE']">		
        <comment>/* CHEMCAM IMAGE HEADER FOOTER TABLE PARAMETERS */</comment>
        <OBJECT>
		<xsl:attribute name="name"><xsl:value-of select="@name"/></xsl:attribute>
			<xsl:apply-templates/>
		</OBJECT>
	</xsl:template>
	
	<!--  SHOULD THIS BE A GROUP ??  -->
	<xsl:template match="PROPERTY[@name='TABLE']">		
        <comment>/* DATA OBJECT */</comment>
        <OBJECT>
		<xsl:attribute name="name"><xsl:value-of select="@name"/></xsl:attribute>
			<xsl:apply-templates/>
		</OBJECT>
	</xsl:template>
	
	<xsl:template match="PROPERTY[@name='IMAGE_HEADER']">		
        <comment>/* IMAGE_HEADER DATA ELEMENTS */</comment>
        <GROUP>
		<xsl:attribute name="name"><xsl:value-of select="@name"/></xsl:attribute>
			<xsl:apply-templates/>
		</GROUP>
	</xsl:template>
	<xsl:template match="PROPERTY[@name='IMAGE_DATA']">		
        <comment>/* IMAGE DATA ELEMENTS */</comment>
        <OBJECT>
        <xsl:attribute name="name">IMAGE</xsl:attribute>
			<xsl:apply-templates/>
		</OBJECT>
	</xsl:template>
	
 
 
<xsl:template match="item"> 

<xsl:variable name="vCaps" 
    			select="'ABCDEFGHIJKLMNOPQRSTUVWXYZ'"/>
 
<xsl:variable name="vNoECaps" 
    			select="'ABCDFGHIJKLMNOPQRSTUVWXYZ'"/> 
    			  			
<xsl:variable name="nonCaps" 
    			select="'abcdefghijklmnopqrstuvwxyz'"/>
    			
<xsl:variable name="nonCapsNoe" 
    			select="'abcdfghijklmnopqrstuvwxyz'"/>
    			
<xsl:variable name="nonLetterNumber1" 
    			select="'!@#$%^*;,?/=+\'"/>

<xsl:variable name="nonLetterNumber2" 
    			select="'!@#$%^*;,?/=\'"/>
    			
<xsl:variable name="nonLetterNumber" 
    			select="'!@#$%^*;:,?/=+\'"/>
    			
<xsl:variable name="digits" 
    			select="'0123456789'"/>
 
 <xsl:variable name="digitsColon" 
    			select="'0123456789:'"/>
    			
<xsl:variable name="digitsPlusMinus" 
    			select="'+-0123456789'"/>   			
  
<xsl:variable name="digitsPlusMinusDot" 
    			select="'+-0123456789.'"/>  
    			  			
<xsl:variable name="floatingPt" 
    			select="'0123456789Ee+-.'"/>
    			
<xsl:if test="@name!='PROPERTY'">  
<xsl:if test="count(subitem)>0">

<xsl:if test="not(contains(@name, '__UNIT'))">
<item>
<xsl:attribute name="name"><xsl:value-of select="@name"/></xsl:attribute>
<xsl:apply-templates/>
</item>
</xsl:if>

</xsl:if>

<xsl:if test="count(subitem)=0">
 	<xsl:choose>
 	<!--  THESE 3 NEED TO PREVENT THE LINE 330 TEST - THESE SHOULD BE ELSE'S OR A CHOOSE '-->
		<xsl:when test="parent::PROPERTY[@name ='IMAGE_DATA'] and contains(@name,'LINES')">
    	<HIDE>remove IMAGE_DATA LINES</HIDE>
    	</xsl:when>
    	
    	<xsl:when test="parent::PROPERTY[@name ='IMAGE_DATA'] and contains(@name,'SAMPLE_BITS')">
    	<HIDE>remove IMAGE_DATA SAMPLE_BITS</HIDE>
    	</xsl:when>
    	
    	<xsl:when test="parent::PROPERTY[@name ='IMAGE_DATA'] and contains(@name,'SAMPLE_TYPE')">
    	<HIDE>remove IMAGE_DATA SAMPLE_TYPE</HIDE>
    	</xsl:when>
   
    
    <xsl:when test="not(contains(@name, '__UNIT'))">
    <xsl:element name="item">
   
    	
    <xsl:choose> 
        <xsl:when test="contains(@name,'__PTR')">       
        <xsl:attribute name="name">^<xsl:value-of select="substring-before(@name, '__PTR')"/></xsl:attribute>  
        </xsl:when>
        
        <!--comment this part when __PTR implementation in place-->
        
        <xsl:when test="@name='MODEL_DESC'">        
        <xsl:attribute name="name">^<xsl:value-of select="@name"/></xsl:attribute>             
        </xsl:when>
                    
        <xsl:otherwise>      
        <xsl:attribute name="name"><xsl:value-of select="@name"/></xsl:attribute>              
        </xsl:otherwise>
     </xsl:choose>  
    
      
    <xsl:if test="contains(following-sibling::item/@name,'__UNIT')">
    
    <xsl:attribute name="name"><xsl:value-of select="@name"/></xsl:attribute>
    <xsl:attribute name="unit"><xsl:value-of select="following-sibling::item[@name]"/></xsl:attribute>
    <!-- <xsl:value-of select="."/>  -->
    </xsl:if>
    
    <xsl:choose>  
    
    	<xsl:when test="contains(@name,'STRUCTURE__PTR')">       	
    	<xsl:attribute name="name">^<xsl:value-of select="substring-before(@name, '__PTR')"/></xsl:attribute>		
		<xsl:attribute name="quoted">true</xsl:attribute>
		<xsl:value-of select="."/>
        </xsl:when>
    	
    	<!-- convert UNK to N/A - the parser can handle N/A but not UNK for integers -->
    	<!--   	
     	<xsl:when test="contains(.,'UNK') and contains(@name,'LINES')"> 	
		<xsl:attribute name="name"><xsl:value-of select="@name"/></xsl:attribute>			
		<xsl:attribute name="quoted">false</xsl:attribute>
		
		<xsl:value-of select="'N/A'"/>
		</xsl:when>
		 -->  
		  	
		  	<!--  add test for  contains(@name,'_ID')"   NON CAPITAL LETTERS AND NON DIGITS AND LETTERS 
		  	<xsl:variable name="vCaps" 
    			select="'ABCDEFGHIJKLMNOPQRSTUVWXYZ'"/>
    			
    			<xsl:variable name="nonCaps" 
    			select="'abcdefghijklmnopqrstuvwxyz'"/>
    			
    			<xsl:variable name="nonLetterNumber" 
    			select="'!@#$%^&*;:,.?/=+\'"/>
    
    <myMark:myMark/>
    <xsl:template match="myMark:*">
      <xsl:param name="arg1"/>
      
      <xsl:if test="contains($vCaps, $arg1)">
		 one . is OK (not quoted) since this is a float 
		 more than one . would require a quote?	
		 2010-07-06T21:57:18.000Z
		 2000-001T12:02:41.997
		 17:37:52
		 substring index starts at 1
		  	-->
		  	
		<xsl:when test="contains(.,'N/A')">       
        <xsl:attribute name="quoted">true</xsl:attribute>
        <xsl:value-of select="."/>
        </xsl:when>		 
		  	 
		<xsl:when test="@name='SOFTWARE_VERSION_ID'">           
        <xsl:attribute name="quoted">true</xsl:attribute>      
        <xsl:value-of select="."/>  
        </xsl:when> 
        
        <xsl:when test="@name='START_IMAGE_ID'">           
        <xsl:attribute name="quoted">false</xsl:attribute>      
        <xsl:value-of select="."/>  
        </xsl:when> 		
        
        <xsl:when test="@name='APPLICATION_PROCESS_ID'">           
        <xsl:attribute name="quoted">false</xsl:attribute>      
        <xsl:value-of select="."/>  
        </xsl:when> 
        
        <xsl:when test="@name='SPACECRAFT_CLOCK_STOP_COUNT'">           
        <xsl:attribute name="quoted">true</xsl:attribute>      
        <xsl:value-of select="."/>  
        </xsl:when> 
        
        <xsl:when test="@name='SPACECRAFT_CLOCK_START_COUNT'">           
        <xsl:attribute name="quoted">true</xsl:attribute>      
        <xsl:value-of select="."/>  
        </xsl:when> 	
        
        <xsl:when test="@name='TELEMETRY_SOURCE_SCLK_START'">           
        <xsl:attribute name="quoted">true</xsl:attribute>      
        <xsl:value-of select="."/>  
        </xsl:when> 	
        
        <xsl:when test="@name='TELEMETRY_SOURCE_START_TIME'">           
        <xsl:attribute name="quoted">false</xsl:attribute>      
        <xsl:value-of select="."/>  
        </xsl:when> 	
        
        <xsl:when test="@name='INSTRUMENT_SERIAL_NUMBER'">           
        <xsl:attribute name="quoted">true</xsl:attribute>      
        <xsl:value-of select="."/>  
        </xsl:when> 
               	
        <xsl:when test="@name='EXPECTED_TRANSMISSION_PATH'">           
        <xsl:attribute name="quoted">true</xsl:attribute>      
        <xsl:value-of select="."/>  
        </xsl:when> 
        
        <xsl:when test="@name='FLIGHT_SOFTWARE_MODE'">           
        <xsl:attribute name="quoted">true</xsl:attribute>      
        <xsl:value-of select="."/>  
        </xsl:when> 
        
        <xsl:when test="@name='PRODUCT_TAG'">           
        <xsl:attribute name="quoted">true</xsl:attribute>      
        <xsl:value-of select="."/>  
        </xsl:when> 
        
        <xsl:when test="@name='VIRTUAL_CHANNEL_ID'">           
        <xsl:attribute name="quoted">true</xsl:attribute>      
        <xsl:value-of select="."/>  
        </xsl:when> 
              
        <xsl:when test="@name='INSTRUMENT_IDLE_TIMEOUT'">           
        <xsl:attribute name="quoted">false</xsl:attribute>      
        <xsl:value-of select="."/>  
        </xsl:when> 
              
        <xsl:when test="@name='TRANSMISSION_PATH'">           
        <xsl:attribute name="quoted">true</xsl:attribute>      
        <xsl:value-of select="."/>  
        </xsl:when> 
        
        <xsl:when test="@name='EXPECTED_TRANSMISSION_PATH'">           
        <xsl:attribute name="quoted">true</xsl:attribute>      
        <xsl:value-of select="."/>  
        </xsl:when> 
        
        <xsl:when test="contains(., 'UNK') or contains(., 'NULL')">       
        <xsl:attribute name="quoted">true</xsl:attribute>
        <xsl:value-of select="."/>
        </xsl:when>	
        
        <xsl:when test="contains(@name,'_ID') ">       
        <xsl:attribute name="quoted">true</xsl:attribute>
        <xsl:value-of select="."/>
        </xsl:when>	
        
        <xsl:when test="contains(@name,'_NAME') ">       
        <xsl:attribute name="quoted">true</xsl:attribute>
        <xsl:value-of select="."/>
        </xsl:when>	
        
        <xsl:when test="contains(@name,'_NUMBER') ">       
        <xsl:attribute name="quoted">false</xsl:attribute>
        <xsl:value-of select="."/>
        </xsl:when>	
        
        <xsl:when test="contains(@name,'_COUNT') ">       
        <xsl:attribute name="quoted">false</xsl:attribute>
        <xsl:value-of select="."/>
        </xsl:when>	
        
        <xsl:when test="contains(@name,'_CNT') ">       
        <xsl:attribute name="quoted">false</xsl:attribute>
        <xsl:value-of select="."/>
        </xsl:when>	 
        <!--  _COUNT ??? , _CNT integer -->
        
        <xsl:when test="contains(@name,'_SOLAR_TIME') ">       
        <xsl:attribute name="quoted">true</xsl:attribute>
        <xsl:value-of select="."/>
        </xsl:when>	
        
        <xsl:when test="contains(@name,'_TIME') ">       
        <xsl:attribute name="quoted">false</xsl:attribute>
        <xsl:value-of select="."/>
        </xsl:when>	
        
        <xsl:when test="contains(@name,'_FLAG') ">       
        <xsl:attribute name="quoted">true</xsl:attribute>
        <xsl:value-of select="."/>
        </xsl:when>	
        
        <!--  could get ridiculous and do (contains(.,'0e-') or contains(.,'1e-') or  -->
        <xsl:when test="contains(.,'e+') and not(contains(.,'.'))">       
        <xsl:attribute name="quoted">false</xsl:attribute>
        <!-- <xsl:value-of select="."/> -->
        	<xsl:call-template name="globalReplace">
  				<xsl:with-param name="outputString" select="."/>
  				<xsl:with-param name="target" select="'e+'"/>
  				<xsl:with-param name="replacement" select="'.0e+'"/>
  			</xsl:call-template>
        </xsl:when>	
        
        
        <!--  could get ridiculous and do (contains(.,'0e-') or contains(.,'1e-') or  -->
        <xsl:when test="contains(.,'e-') and not(contains(.,'.'))">       
        <xsl:attribute name="quoted">false</xsl:attribute>
        <!-- <xsl:value-of select="."/> -->
        	<xsl:call-template name="globalReplace">
  				<xsl:with-param name="outputString" select="."/>
  				<xsl:with-param name="target" select="'e+'"/>
  				<xsl:with-param name="replacement" select="'.0e-'"/>
  			</xsl:call-template>
        </xsl:when>	
        
        <xsl:when test="(contains(substring(.,11,1), 'T') and contains(substring(.,14,1), ':')  and contains(substring(.,8,1), '-'))">       
        <xsl:attribute name="quoted">false</xsl:attribute>
        <xsl:value-of select="."/>
        </xsl:when>	 
        
        <xsl:when test="(contains(substring(.,9,1), 'T') and contains(substring(.,12,1), ':')  and contains(substring(.,5,1), '-'))">       
        <xsl:attribute name="quoted">false</xsl:attribute>
        <xsl:value-of select="."/>
        </xsl:when>	 
        
        <xsl:when test="(contains(substring(.,3,1), ':') and contains(substring(.,6,1), ':') )">      
        <xsl:attribute name="quoted">false</xsl:attribute>
        <xsl:value-of select="."/>
        </xsl:when>	 
        
        <xsl:when test="(contains($digitsPlusMinus,substring(.,1,1)) and not(contains(., ' ')) )">       
        <xsl:attribute name="quoted">false</xsl:attribute>
        <xsl:value-of select="."/>
        </xsl:when>		
              		  	
		<xsl:when test="contains(., ' ') or contains(., '-') or contains(., '.TXT')
		       or contains($nonCaps,substring(.,1,1))">       
        <xsl:attribute name="quoted">true</xsl:attribute>
        <xsl:value-of select="."/>
        </xsl:when>	
		  	
		
		  	
		 <!--  matches is an xslt 2.0 construct we are using 1.0 
         <xsl:when test="matches(., '[a-z]')">
		  <xsl:attribute name="quoted">true</xsl:attribute>      
        <xsl:value-of select="."/>  
        </xsl:when> 	
        
        
        <xsl:when test="contains(., ' ') or contains(., '-') or contains(., '.TXT') or contains(.,'N/A') or contains(., 'UNK') or contains(., 'NULL')">       
        <xsl:attribute name="quoted"><xsl:value-of select="@quoted"/></xsl:attribute>
        <xsl:value-of select="."/>
        </xsl:when>
        -->
        
        <xsl:otherwise>      
        <xsl:attribute name="name"><xsl:value-of select="@name"/></xsl:attribute>
        <xsl:attribute name="quoted">false</xsl:attribute>
        <xsl:value-of select="."/>
        </xsl:otherwise>
    </xsl:choose>
    </xsl:element>
    </xsl:when> 
 </xsl:choose>
</xsl:if>
</xsl:if>


</xsl:template>  


<xsl:template match="subitem">
<xsl:variable name="vCaps" 
    			select="'ABCDEFGHIJKLMNOPQRSTUVWXYZ'"/>
    			
<xsl:variable name="nonCaps" 
    			select="'abcdefghijklmnopqrstuvwxyz'"/>
    			
<xsl:variable name="nonLetterNumber" 
    			select="'!@#$%^*;:,.?/=+\'"/>

<xsl:variable name="digits" 
    			select="'0123456789'"/>

<xsl:variable name="digitsPlusMinus" 
    			select="'+-0123456789'"/>   
    			    			
<xsl:element name="subitem">
<xsl:choose>


        <xsl:when test="contains(.,'N/A') or contains(., 'UNK') or contains(., 'NULL')">       
        <xsl:attribute name="quoted">true</xsl:attribute>
        <xsl:value-of select="."/>
        </xsl:when>	

		<xsl:when test="contains(@name,'_NAME') ">       
        <xsl:attribute name="quoted">true</xsl:attribute>
        <xsl:value-of select="."/>
        </xsl:when>	
        
        <xsl:when test="contains(@name,'_ID') ">       
        <xsl:attribute name="quoted">true</xsl:attribute>
        <xsl:value-of select="."/>
        </xsl:when>	
        
        <xsl:when test="contains(@name,'_NUMBER') ">       
        <xsl:attribute name="quoted">false</xsl:attribute>
        <xsl:value-of select="."/>
        </xsl:when>	
        
        <xsl:when test="contains(@name,'_TIME') ">       
        <xsl:attribute name="quoted">false</xsl:attribute>
        <xsl:value-of select="."/>
        </xsl:when>	
        
        <xsl:when test="contains(@name,'_FLAG') ">       
        <xsl:attribute name="quoted">true</xsl:attribute>
        <xsl:value-of select="."/>
        </xsl:when>	
        
        <xsl:when test="contains(@name,'_COUNT') ">       
        <xsl:attribute name="quoted">false</xsl:attribute>
        <xsl:value-of select="."/>
        </xsl:when>	
        
        <xsl:when test="contains(@name,'_CNT') ">       
        <xsl:attribute name="quoted">false</xsl:attribute>
        <xsl:value-of select="."/>
        </xsl:when>	
        
        <xsl:when test="contains(.,'e+') and not(contains(.,'.'))">       
        <xsl:attribute name="quoted">false</xsl:attribute>
        <!-- <xsl:value-of select="."/> -->
        	<xsl:call-template name="globalReplace">
  				<xsl:with-param name="outputString" select="."/>
  				<xsl:with-param name="target" select="'e+'"/>
  				<xsl:with-param name="replacement" select="'.0e+'"/>
  			</xsl:call-template>
        </xsl:when>	
        
        <xsl:when test="contains(.,'e-') and not(contains(.,'.'))">       
        <xsl:attribute name="quoted">false</xsl:attribute>
        <!-- <xsl:value-of select="."/> -->
        	<xsl:call-template name="globalReplace">
  				<xsl:with-param name="outputString" select="."/>
  				<xsl:with-param name="target" select="'e+'"/>
  				<xsl:with-param name="replacement" select="'.0e-'"/>
  			</xsl:call-template>
        </xsl:when>	
        	
 		<xsl:when test="(contains(substring(.,11,1), 'T') and contains(substring(.,14,1), ':')  and contains(substring(.,8,1), '-'))">       
        <xsl:attribute name="quoted">false</xsl:attribute>
        <xsl:value-of select="."/>
        </xsl:when>	 
        
        <xsl:when test="(contains(substring(.,9,1), 'T') and contains(substring(.,12,1), ':')  and contains(substring(.,5,1), '-'))">       
        <xsl:attribute name="quoted">false</xsl:attribute>
        <xsl:value-of select="."/>
        </xsl:when>	 
        
        <xsl:when test="(contains(substring(.,3,1), ':') and contains(substring(.,6,1), ':') )">      
        <xsl:attribute name="quoted">false</xsl:attribute>
        <xsl:value-of select="."/>
        </xsl:when>	 	
        
        
        <xsl:when test="(contains($digitsPlusMinus,substring(.,1,1)) and not(contains(., ' ')) )">       
        <xsl:attribute name="quoted">false</xsl:attribute>
        <xsl:value-of select="."/>
        </xsl:when>		
              		  	
		
        <xsl:when test="contains(., ' ') or contains(., '-') or contains(., '.TXT') 
		       or contains($nonCaps,substring(.,1,1))">       
        <xsl:attribute name="quoted">true</xsl:attribute>
        <xsl:value-of select="."/>
        </xsl:when>	
		  	
		
    
    <xsl:otherwise>
    <xsl:attribute name="quoted">false</xsl:attribute> 
    <xsl:value-of select="."/>
    </xsl:otherwise>
    
</xsl:choose>
<xsl:choose>

<xsl:when test="contains(../following-sibling::item/subitem/@name, '__UNIT')">
<xsl:attribute name="unit"><xsl:value-of select="../following-sibling::item/subitem[@name]"/></xsl:attribute>
</xsl:when>
</xsl:choose>
<!--  <xsl:value-of select="."/> -->
</xsl:element>
</xsl:template>

<xsl:template name="globalReplace">
  <xsl:param name="outputString"/>
  <xsl:param name="target"/>
  <xsl:param name="replacement"/>
  <xsl:choose>
    <xsl:when test="contains($outputString,$target)">
   
      <xsl:value-of select=
        "concat(substring-before($outputString,$target),
               $replacement)"/>
      <xsl:call-template name="globalReplace">
        <xsl:with-param name="outputString" 
             select="substring-after($outputString,$target)"/>
        <xsl:with-param name="target" select="$target"/>
        <xsl:with-param name="replacement" 
             select="$replacement"/>
      </xsl:call-template>
    </xsl:when>
    <xsl:otherwise>
      <xsl:value-of select="$outputString"/>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>





</xsl:stylesheet>


