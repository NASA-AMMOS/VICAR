class MiniHeader
{
   protected:

    unsigned int product_id_;
    unsigned char thumbnail_;
    unsigned int magic0_;
    unsigned int sclk_;
    unsigned int minisclk_;
    unsigned short vflush_;
    unsigned short mode_;
    unsigned char filter_;
    float exposure_;
    unsigned char window_x_;
    unsigned char window_y_;
    unsigned char width_;
    unsigned char height_;
    unsigned int autofocus_;
    unsigned short initial_position_;
    unsigned short step_size_;
    unsigned short af_n_steps_;
    unsigned short zstack_;
    unsigned int autoexposure_;
    unsigned char target_dn_;
    unsigned char fraction_;
    unsigned char early_termination_;
    unsigned char ae_n_steps_;

    unsigned int start_image_id_;
    unsigned char exposure_count_;
    unsigned int z_stack_parms_;
    
    unsigned char compression_[8];
    unsigned char camera_status_;
    unsigned int sys_serial_no_;
    unsigned char mech_[8];
    unsigned int mech0_;
    unsigned short mech1_;
    unsigned short mech2_;
    unsigned int dc_offset_;
    unsigned int initsize_;
    unsigned int magic1_; 
    int comp_quality_;

    bool isZstack_;
    bool isVideo_;
    bool isLossless_;
    bool isColor_;

  public:

    MiniHeader() {}


    int parse_mini_header(unsigned char *miniHeader_, int isZstack);

    virtual unsigned int getProductId() const {return this->product_id_;}
    virtual unsigned char getThumbnail() const {return this->thumbnail_;}
    virtual unsigned int getMagic0() const {return this->magic0_;}
    virtual unsigned int getSclk() const {return this->sclk_;}
    virtual unsigned int getMiniSclk() const {return this->minisclk_;}
    virtual unsigned short getVFlush() const {return this->vflush_;}
    virtual unsigned short getMode() const {return this->mode_;}
    virtual unsigned char getFilter() const {return this->filter_;}
    virtual float getExposure() const {return this->exposure_;}
    virtual unsigned char getWindowX() const {return this->window_x_;}
    virtual unsigned char getWindowY() const {return this->window_y_;}
    virtual unsigned char getWidth() const {return this->width_;}
    virtual unsigned char getHeight() const {return this->height_;}
    virtual unsigned int getAutofocus() const {return this->autofocus_;}
    virtual unsigned short getInitialPosition() const {return this->initial_position_;}
    virtual unsigned short getStepSize() const {return this->step_size_;}
    virtual unsigned short getAFNSteps() const {return this->af_n_steps_;}
    virtual unsigned short getZstack() const {return this->zstack_;}
    virtual unsigned int getAutoexposure() const {return this->autoexposure_;}
    virtual unsigned char getTargetDN() const {return this->target_dn_;}
    virtual unsigned char getFraction() const {return this->fraction_;}
    virtual unsigned char getEarlyTermination() const {return this->early_termination_;}
    virtual unsigned char getAENSteps() const {return this->ae_n_steps_;}
    virtual unsigned int getStartImageId() const {return this->start_image_id_;}
    virtual unsigned char getExposureCount() const {return this->exposure_count_;}
    virtual unsigned int getZstackParms() const {return this->z_stack_parms_;}
    virtual unsigned char* getCompression() {return this->compression_;}
    virtual int getCompQuality() const {return this->comp_quality_;}
    virtual unsigned char getCameraStatus() const {return this->camera_status_;}
    virtual unsigned int getSysSerialNo() const {return this->sys_serial_no_;}
    virtual const unsigned char* getMech() const {return this->mech_;}
    virtual unsigned int getMech0() const {return this->mech0_;}
    virtual unsigned short getMech1() const {return this->mech1_;}
    virtual unsigned short getMech2() const {return this->mech2_;}
    virtual unsigned int getDCOffset() const {return this->dc_offset_;}
    virtual unsigned int getInitSize() const {return this->initsize_;}
    virtual unsigned int getMagic1() const {return this->magic1_;}
    virtual unsigned isZstack() const {return this->isZstack_;}
    virtual bool isLossless() const {return this->isLossless_;}
    virtual bool isColor() const {return this->isColor_;}
    virtual unsigned isVideo() const {return this->isVideo_;}

};

