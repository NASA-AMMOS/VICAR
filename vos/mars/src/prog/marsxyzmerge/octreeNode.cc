#include <iostream>
#include <vector>

#include "octreeNode.h"

using namespace std;


OctreeNode::OctreeNode(vector<vector<Point> > * points) : points(points)
{
   double Xmin, Xmax, Ymin, Ymax, Zmin, Zmax;
   Xmin = Ymin = Zmin = 1e20;
   Xmax = Ymax = Zmax = -1e20;
   Point point;

   // Initialize the children octants to NULL
   for (int i=0; i<8; i++)
      children[i] = NULL;

   // Get the most outside points (in terms of coordinates) to define the size
   // and origin (central point) of the octant 
   for (int i=0; i<points->size(); i++) {
      for (int j=0; j<(*points)[i].size(); j++) {
         point = (*points)[i][j];
         if (Xmin > point.X) Xmin = point.X;
         if (Xmax < point.X) Xmax = point.X;
         if (Ymin > point.Y) Ymin = point.Y;
         if (Ymax < point.Y) Ymax = point.Y;
         if (Zmin > point.Z) Zmin = point.Z;
         if (Zmax < point.Z) Zmax = point.Z;
      }
   }

   // Position the center of the octant at the center of the point clouds
   center[0] = (Xmax-Xmin)/2.0 + Xmin;
   center[1] = (Ymax-Ymin)/2.0 + Ymin;
   center[2] = (Zmax-Zmin)/2.0 + Zmin;

   // Get the length of the octant side. Because an octant is a cube, the length
   // is the maximum of the X,Y,Z range
   length = Xmax-Xmin;
   if ((Ymax-Ymin) > length) length = Ymax-Ymin; 
   if ((Zmax-Zmin) > length) length = Zmax-Zmin;
}


OctreeNode::OctreeNode(double in_center[], double in_length, int nbPointClouds)
{
   for (int i=0; i<8; i++)
      children[i] = NULL;

   for (int i=0; i<3; i++)
    center[i] = in_center[i];

   length = in_length;
   
   points = new vector<vector<Point> >(nbPointClouds);
}


OctreeNode::~OctreeNode()
{
   // Empty the points vectors
   for (int i=0; i<points->size(); i++)
     (*points)[i].clear();
   points->clear();

   // Delete the children - It is a recursive process
   for (int i=0; i<8; i++) {
      if (children[i] != NULL)
         delete children[i];
   }

}


OctreeNode * OctreeNode::getChild(int index)
{
   if (index > 7) 
      return NULL;

   return children[index];
}



void OctreeNode::addPoint(Point point, int index)
{
   (*points)[index].push_back(point);
}


bool OctreeNode::isLeaf()
{
   if (children[0] == NULL)
      return true;
   else
      return false;
}


vector<int> OctreeNode::getPointCloudsSize()
{
   vector<int> pointClouds;

   for(int i=0; i<points->size(); i++)
      pointClouds.push_back((*points)[i].size());

   return pointClouds;
}


void OctreeNode::clearPointCloud(int i)
{
   // Although the clear() function is what is needed here: (*points)[i].clear()
   // the memory is nor effectively released. Instead using swap with a non
   // existing vector does release the memory.
    vector<Point>().swap((*points)[i]);
}



void OctreeNode::getPointClouds(vector<vector<Point> > & pointClouds)
{
   if (children[0] == NULL) {
      for (int i=0; i<pointClouds.size(); i++)
         pointClouds[i].insert(pointClouds[i].end(), (*points)[i].begin(), (*points)[i].end()); 
      return;
   }
   
   for (int i=0; i<8; i++)
      children[i]->getPointClouds(pointClouds);
}



int OctreeNode::split()
{
   int loc;
   Point point;

   // Initialize the children octants
   for (int i=0; i<8; i++) {
      double centerChild[3];
      centerChild[0] = center[0] + length/4.0 * (i&4 ? 1.0 : -1.0);
      centerChild[1] = center[1] + length/4.0 * (i&2 ? 1.0 : -1.0);
      centerChild[2] = center[2] + length/4.0 * (i&1 ? 1.0 : -1.0);
      children[i] = new (std::nothrow) OctreeNode(centerChild, 
                                                  length/2.0, 
                                                  points->size());  
      if (children[i] == NULL)
         // Failure
         return 1;
   }

   // Iterate over the different point clouds and attribute point to correct 
   // children node depending on its spatial position
   for (int i=0; i<points->size(); i++) {
      for (int j=0; j<(*points)[i].size(); j++) {
         loc = 0;
         point = (*points)[i][j];
         if (point.X > center[0]) loc = 4;          
         if (point.Y > center[1]) loc += 2;          
	 if (point.Z > center[2]) loc += 1;          
         (children[loc])->addPoint(point,i);
      }
   }

   // Now that all the points have been attributed to a child octree, we delete
   // points in the parent node to free some memory. Again here, using swap() 
   // instead of clear() to effectively release the memory
   for (int i=0; i<points->size(); i++)
      vector<Point>().swap((*points)[i]);

   // Success
   return 0;
}

