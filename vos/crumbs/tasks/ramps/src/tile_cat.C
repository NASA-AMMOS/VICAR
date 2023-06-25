///////////////////////////////////////////////////////////////////////////
// tile_cat.C
// 
// Program reads in single Inventor file that might contain FILE references
// and multiple levels of detail; outputs single Inventor file that doesn't
// contain any FILE references, doesn't contain any duplicate definitions
// of textures, and retains only the highest level of detail.
//
// Possible problem with texture definition removal: there is no check for
// whether two textures are the same; if the program sees any multiple
// textures, it will remove all but one of them.
//
// by Oleg Pariser and Jacob Rodeheffer
// 8/22/13
//
/////////////////////////////////////////////////////////////////////////

class SoTempPath;

#include <Inventor/SoDB.h>
#include <Inventor/SoInput.h>
#include <Inventor/nodes/SoNode.h>
#include <Inventor/nodes/SoTexture2.h>
#include <Inventor/nodes/SoLevelOfDetail.h>
#include <Inventor/SoLists.h>
#include <Inventor/actions/SoSearchAction.h>
#include <Inventor/actions/SoWriteAction.h>
#include <Inventor/nodes/SoFile.h>
#include <Inventor/nodes/SoSeparator.h>
#include <Inventor/nodekits/SoBaseKit.h>

#include <stdlib.h>
#include <stdio.h>

void integrateFiles(SoSeparator* root);
void mergeDuplicateTextures(SoSeparator* root);
void demandHighestLODs(SoSeparator* root);


int main(int argc, char **argv) {

	SoDB::init();
	SoInput mySceneInput;
  
	// Correct usage check
	if ( argc > 7 || argc < 4 ) /* argc should be 4, 5, 6, or 7 for correct execution */ {
		/* We print argv[0] assuming it is the program name */
		printf( "usage: %s work_dir filename_input filename_output [bin] [tex] [lod]\n", argv[0] );
		printf( "bin means write output in binary format. tex means merge all textures. lod means remove all low levels of detail.\n" );
		return 0;
	}

	// Process command-line options
	bool write_binary = false;
	bool merge_textures = false;
	bool select_highest_lod = false;
	for (int i = 4; argc >= i+1; i++) {
		write_binary = ( strcmp(argv[i], "bin") == 0 ) || write_binary;
		merge_textures = ( strcmp(argv[i], "tex") == 0 ) || merge_textures;
		select_highest_lod = ( strcmp(argv[i], "lod") == 0 ) || select_highest_lod;
		if (!write_binary && !merge_textures && !select_highest_lod) {
			fprintf(stderr, "ERROR: unrecognized command-line argument\n");
			return 0;
		}
	}
	

	// Read the whole file into the scene graph database
	mySceneInput.addDirectoryFirst(argv[1]);
	if (!mySceneInput.openFile(argv[2])) {
		fprintf(stderr, "Cannot open file %s\n", argv[2]);
		return 0;
	}
	SoSeparator* myGraph = SoDB::readAll(&mySceneInput);
	if (myGraph == NULL) {
		fprintf(stderr, "Problem reading file\n");
		return 0;
	}
	mySceneInput.closeFile();

	// Create scene graph root, add input file data as child
	SoSeparator* root = new SoSeparator();
	root->ref();
	root->addChild(myGraph);
    
    
	// Operate on the open scene graph
	integrateFiles(root);
	if (merge_textures) {
		mergeDuplicateTextures(root);
	}
	if (select_highest_lod) {
		demandHighestLODs(root);
	}


	// Write database to output file
	if (write_binary) {
		fprintf(stderr, "Writing out file %s in binary format\n", argv[3]);
	}
	else {
		fprintf(stderr, "Writing out file %s in ASCII format\n", argv[3]);
	}
	SoWriteAction writeAction;
	writeAction.getOutput()->openFile( argv[3] );
	writeAction.getOutput()->setBinary( write_binary );
	writeAction.apply( root );
	writeAction.getOutput()->closeFile();
}


// CUSTOM FUNCTION DEFS -------------------------


void integrateFiles(SoSeparator* root) {
	// Find all SoFile node paths
	SoSearchAction sa;
	sa.setType(SoFile::getClassTypeId() );
	sa.setInterest(SoSearchAction::ALL );
	sa.setSearchingAll(TRUE);
	SoBaseKit::setSearchingChildren(TRUE);
	sa.apply(root);
	SoPathList &pathList = sa.getPaths();
	int numPaths = pathList.getLength();
	fprintf(stderr, "Number of .iv file references found = %d\n", numPaths);
    
	// Replace SoFile nodes with regular SoGroup nodes
	for (int i = 0; i < numPaths; i++) {
		// Get next SoFile node in list
		SoFullPath *pPath = (SoFullPath*)pathList[i];
		SoFile *pFile = (SoFile*)pPath->getTail();
	
		// Get the root(s) of the SoFile's scene graph and add them as children to a new SoGroup
		SoNodeList *childList = (SoNodeList* )pFile->getChildren();
		SoGroup *newChildGroup = new SoGroup;
		newChildGroup->ref();
		for (int j=0; j < childList->getLength(); ++j) {
			newChildGroup->addChild(( *childList)[j]);
		}
        
		// Replace the SoFile node in the scene graph with newChildGroup
		pFile->ref();
		SoGroup* parentNode = (SoGroup *)pPath->getNodeFromTail(1);
		parentNode->replaceChild(pFile, newChildGroup);
		pFile->unref();
		newChildGroup->unref();
	}
    
	// Report on success
	sa.apply(root);
	SoPathList& pathList2 = sa.getPaths();
	numPaths = pathList2.getLength();
	fprintf(stderr, "Number of .iv file references remaining = %d\n", numPaths);
}


void mergeDuplicateTextures(SoSeparator* root) {
	// POSSIBLE PROBLEM: does not check whether multiple textures are really the same; treats all multiple textures as duplicates and removes them

	// Find all SoTexture2 node paths
	SoSearchAction sa;
	sa.setType( SoTexture2::getClassTypeId() );
	sa.setInterest( SoSearchAction::ALL );
	sa.setSearchingAll(true);
	SoBaseKit::setSearchingChildren(true);
	sa.apply(root);
	SoPathList& pathList = sa.getPaths();
	int numPaths = pathList.getLength();
    
	SoTexture2** texture_list = new SoTexture2*[numPaths];
	int unique_texture_count = 0;
    
	// Build list of texture pointers, each time checking for uniqueness against already-listed pointers
	for (int i = 0; i < numPaths; i++) {
		SoFullPath* path = (SoFullPath*)pathList[i];
		texture_list[i] = (SoTexture2*)path->getTail();
		unique_texture_count++;
		for (int j = 0; j < i; j++) {
			if (texture_list[j] == texture_list[i]) {
				unique_texture_count--;
				break;
			}
		}
	}
    
	fprintf(stderr, "Number of texture definitions found = %d\n", unique_texture_count);
  
	SoNode* texture_node;
    
	// Eliminate duplicate textures
	for (int i = 0; i < numPaths; i++) {
		// Get the next SoTexture2 node in list
		SoFullPath* path = (SoFullPath*)pathList[i];
		SoTexture2* pTexture2 = (SoTexture2*)path->getTail();

		if (i == 0) {
			texture_node = pTexture2;
		}
		else {
			// Replace pTexture2 in the scene graph with texture_node
			SoGroup* parentNode = (SoGroup*)path->getNodeFromTail(1);
			parentNode->replaceChild(pTexture2, texture_node);
		}
	}
    
	// Report on success
	sa.apply(root);
	SoPathList& pathList2 = sa.getPaths();
	numPaths = pathList2.getLength();
	unique_texture_count = 0;
    
	// Build list of texture pointers, each time checking for uniqueness against already-listed pointers
	for (int i = 0; i < numPaths; i++) {
		SoFullPath* path = (SoFullPath*)pathList[i];
		texture_list[i] = (SoTexture2*)path->getTail();
		unique_texture_count++;
		for (int j = 0; j < i; j++) {
			if (texture_list[j] == texture_list[i]) {
				unique_texture_count--;
				break;
			}
		}
	}
    
	fprintf(stderr, "Number of texture definitions remaining = %d\n", unique_texture_count);
    
	delete [] texture_list;
}


void demandHighestLODs(SoSeparator* root) {
	// Find all SoLevelOfDetail node paths
	SoSearchAction sa;
	sa.setType( SoLevelOfDetail::getClassTypeId() );
	sa.setInterest(SoSearchAction::ALL );
	sa.setSearchingAll(true);
	SoBaseKit::setSearchingChildren(true);
	sa.apply(root);
	SoPathList& pathList = sa.getPaths();
	int numPaths = pathList.getLength();
	
	// For each SoLevelOfDetailNode, remove all but the first (highest-detail) child
	for (int i = 0; i < numPaths; i++) {
		// Get the next SoLevelOfDetail node
		SoFullPath* path = (SoFullPath*)pathList[i];
		SoLevelOfDetail* lod_node = (SoLevelOfDetail*)path->getTail();
		int original_child_count = lod_node->getNumChildren();
	
		for (int j = 1; j < original_child_count; j++) {
			lod_node->removeChild(1);
		}
		
		fprintf(stderr, "LOD group %d, # of children before/after: %d,%d\n", i+1, original_child_count, lod_node->getNumChildren());
	}
}





