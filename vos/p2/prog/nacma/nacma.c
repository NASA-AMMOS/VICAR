#include "vicmain_c.h"
#include "applic.h"

#include "cloud_masks.h"
#include "ImageUtils.h"
#include "AsterManager.h"
#include "NACMA_Utils.h"

/******************************************************************************/
void main44(void)
{
   NACMA_STRUCT *nacma;
   CLOUD_MASKS *masks;

   zifmessage("*** nacma version 2017-08-09 ***");

   // preprocess2
   nacma = getNACMA();

   createRadianceFiles(nacma);
   createReflectanceFiles(nacma);
   reopenReflectanceFiles(nacma);
   createBTempFiles(nacma);
   createDownSampledFiles(nacma);
   reopenFilesForPass1(nacma);

   // process pass-1
   masks = get_CLOUD_MASKS(nacma->bTemp13Image->nl, nacma->bTemp13Image->ns);
   setBandImages(&masks,
		 nacma->ref1Image,
		 nacma->ref2Image,
		 nacma->ref3Image,
		 nacma->ref4Image,
		 nacma->bTemp13Image);
   setWorkspaceImages(&masks, nacma->ndsiImage, nacma->tempCompImage,
                      nacma->gvImage, nacma->svImage, nacma->rsImage);
   init_CM_WORKSPACE(&masks);
   doPass1(masks);
   doPass2(masks);

   createMaskFiles(nacma, masks);

   deleteNACMA(nacma);
}
