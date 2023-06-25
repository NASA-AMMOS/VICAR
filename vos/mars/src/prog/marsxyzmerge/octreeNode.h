#include <iostream>
#include <vector>
using namespace std;

// Structure to store a point cloud point
struct Point {
    float X; // X,Y,Z coordinates of point
    float Y;
    float Z;
    long index; // pixel localisation (index=line*lineSize+sample)
};

// Octree class used to store and manage point cloud points
class OctreeNode
{ 
   private:

     // The eight child octants of a octree node
     OctreeNode * children[8];

     // Container for the actual points. A vector of vector. The first dimension
     // is equal to the number of point cloud files. The second dimension stores
     // all the points for a given point cloud
     vector<vector<Point> > * points; 

     // Coordinates of the center of the Node
     double center[3];
     
     // Length of the node box, in same unit as the points
     double length;


   public:

     // Constructor, usually for the initialization of the root/parent octant.
     // It takes a vector of vector as input, with the first dimension equals to
     // the number of point cloud sources, and the second dimension being the 
     // points themselves
     OctreeNode(vector<vector<Point> > * points);

     // Constructor, usually for the initialization of a child of an existing 
     // octant.
     OctreeNode(double * center, double length, int nbPointClouds);

     // Destructor
     ~OctreeNode();

     // Return the child octant of the Node at the index location (0->7)
     OctreeNode * getChild(int index);
     
     // Add a point to node. Index indicates in which point cloud source the 
     // point is to be added
     void addPoint(Point point, int index);

     // The split function splits the node into its 8 octants and fill each
     // octants with the corresponding points. Each octant will have exactly the
     // same number of point clouds sources as the parent (the same number of
     // point cloud sources, not the same number of points!), although some will
     // could be empty. 
     int split();

     // Return true if node is leaf, otherwise the node is a branch
     bool isLeaf();

     // Return a vector of size the number of point clouds, and with each
     // elements of the vector equals to the number of points from the 
     // corresponding point cloud (index wise). For instance if the octant 
     // contains 3 points clouds, with 10, 0, 30 the number of points from 
     // point cloud 1,2,3 respectively, then returned vector will be: {10,0,30}
     vector<int> getPointCloudsSize();

     // Return a vector of vector containing all the points present in the 
     // current node. If current node is a branch, the returned vector 
     // combines all the points contained in the descendants of the current
     // node
     void getPointClouds(vector<vector<Point> > & pointClouds);

     // Remove all points from the point cloud indicated by "index"
     void clearPointCloud(int index);
};


