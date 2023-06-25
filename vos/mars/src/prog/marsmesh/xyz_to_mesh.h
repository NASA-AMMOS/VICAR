#include "PigCameraModel.h"

#include "grape/object.h"
#include "grape/sfcmodel.h"


// Add candidate triangle if normal vector is okay
// (indicating face is visible from eye point [camera])
// Note all calculations are in model (octree) space.
int try_mesh(NodeSpec *v1, NodeSpec *v2, NodeSpec *v3,
             Triangle_Model *tm, double max_angle, double mdlcam[3]);

void add_mesh(NodeSpec *ulns, NodeSpec *urns, NodeSpec *llns,
              NodeSpec *lrns, Triangle_Model *tm, double max_angle, double mdlcam[3]);

void vertex_texture(NodeSpec *ns, PigCameraModel* camera_model, double c_pt[3],
                    ZMatrix modelToWorld, int xres, int yres, double st[2]);
