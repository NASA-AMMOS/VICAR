#include "xyz_to_mesh.h"

// Add candidate triangle if normal vector is okay
// (indicating face is visible from eye point [camera])
// Note all calculations are in model (octree) space.
int try_mesh(NodeSpec *v1, NodeSpec *v2, NodeSpec *v3,
             Triangle_Model *tm, double max_angle, double mdlcam[3])
{
        // Max ratio of longest triangle edge to sum of shorter edges.
        // Triangles exceeding this ratio are candidate slivers
        double sliver_ratio = 0.95;

        // Max cosine of angle between long triangle edge and vector to eye.
        // Triangles exceeding this cosine
        // (i.e. edges within 1 degree of eye vector) are discarded as slivers
        double sliver_cos = 0.9998;

        // Minimum cosine of angle between face normal and eye vector. Smaller
        // cosine (larger angle) indicates invalid face triangle.
        //double face_threshold = 0.08716; // default = 85 degrees
        double face_threshold = cos(TORADIANS * max_angle);

        int sliver_count=0;                       // diagnostic (sliver rejects)
        int angle_count=0;                        // diagnostic (angle rejects)

        double to_eye[3];

        // Check for sliver triangle:
        // First, is longest edge close to sum of smaller edges?
        double p1[3], p2[3], p3[3], *pa, *pb;
        v1->get_global_center(p1);
        v2->get_global_center(p2);
        v3->get_global_center(p3);
        double len1 = distance(p1, p2);
        double len2 = distance(p2, p3);
        double len3 = distance(p3, p1);
        double ratio;
        if (len1 > len2) {
                if (len1 > len3) {      // 1 is longest
                        ratio = len1 / (len2 + len3);
                        pa = p1; pb = p2;
                } else {                // 3 is longest
                        ratio = len3 / (len1 + len2);
                        pa = p3; pb = p1;
                }
        } else if (len2 > len3) {       // 2 is longest
                ratio = len2 / (len1 + len3);
                pa = p2; pb = p3;
        } else {                        // 3 is longest
                ratio = len3 / (len1 + len2);
                pa = p3; pb = p1;
        }
        if (ratio > sliver_ratio) {
                // have candidate (pa-pb);
                // is edge nearly colinear with eye point?
                double edge_unit[3];
                get_vector(pa, pb, edge_unit);
                get_vector(pa, mdlcam, to_eye);
                if (fabs(dot_product(edge_unit, to_eye)) > sliver_cos) {
                        sliver_count++;
                        return FALSE;
                }
        } else {
                // get unit vector from any vertex to eye point
                get_vector(p1, mdlcam, to_eye);
        }

        // get angle between eye vector and triangle normal
        Mesh_Triangle *mt = new Mesh_Triangle(v1, v3, v2);
        double n[3];
        mt->tri_surface_normal(n);
        double cos_angle = dot_product(n, to_eye);
        if (cos_angle < face_threshold) {
                delete mt;
                angle_count++;
                return FALSE;
        }

        // okay, add triangle to the mesh
        tm->add_triangle(mt);
        return TRUE;
}

// Possibly add triangles to mesh for given set of four points.
// Invalid points have NULL nodespec pointers.
// ulns ----- urns
//   |         |
//   |         |
// llns ----- lrns
void add_mesh(NodeSpec *ulns, NodeSpec *urns, NodeSpec *llns,
              NodeSpec *lrns, Triangle_Model *tm, double max_angle, double mdlcam[3])
{
        // count valid points
        int nvalid = (ulns != NULL) + (urns != NULL) + (llns != NULL) +
                (lrns != NULL);

        if (nvalid == 4) {              // all points valid, two triangles
                // try splitting on LL-UR diagonal
                nvalid = try_mesh(ulns, urns, llns, tm, max_angle, mdlcam) +
                         try_mesh(urns, lrns, llns, tm, max_angle, mdlcam);
                if (nvalid == 0) {      // no good, try UL-LR diagonal
                        try_mesh(ulns, urns, lrns, tm, max_angle, mdlcam);
                        try_mesh(ulns, lrns, llns, tm, max_angle, mdlcam);
                }

        } else if (nvalid == 3) {       // only one triangle
                if (ulns == NULL)
                        try_mesh(urns, lrns, llns, tm, max_angle, mdlcam);
                else if (urns == NULL)
                        try_mesh(ulns, lrns, llns, tm, max_angle, mdlcam);
                else if (llns == NULL)
                        try_mesh(ulns, urns, lrns, tm, max_angle, mdlcam);
                else
                        try_mesh(ulns, urns, llns, tm, max_angle, mdlcam);
        }
}


void vertex_texture(NodeSpec *ns, PigCameraModel* camera_model,double c_pt[3],
                    ZMatrix modelToWorld, int xres, int yres, double st[2]) {

        // get coordinates in patch's camera frame
        double gc[3], cc[3];
        ns->get_global_center(gc);
   
        PigPoint pt;
        double line, sample;
        MultPoints(gc, modelToWorld, cc);
        pt.setXYZ(cc);
        // use camera model to project 3D point to image
        //pi->p->cmod.To_2D(cc, st);
        camera_model->XYZtoLS(pt, 0, &line, &sample, NULL);
        // convert pixel->fraction, deal with left-handedness
        st[0] = sample / xres;
        st[1] = 1.0 - line / yres;
    return;
}
