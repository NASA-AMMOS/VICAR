// metarender.h

#ifndef	_METARENDER_H_
#define _METARENDER_H_

#ifdef DO_GL
#include "grape/glrend.h"
#endif
#include "grape/oglrend.h"
#include "grape/playback.h"
#include "grape/hazerend.h"

// The class for the meta renderer.
// 
class MetaRenderer : public Renderer
   {
   private:

   protected:

	Renderer	**rndr_list;
	int		num_rndrs;
	Scene		**scene_list;
	int		num_scenes;
	ImgInfo		**info_list;
	int		num_infos;
	Renderer	**post_rndr_list;
	int		num_post_rndrs;

	void		add_rndr(Renderer *s) {
		num_rndrs++;
		rndr_list = (Renderer **)realloc((void *)rndr_list, (num_rndrs+1) * sizeof(Renderer *));
		rndr_list[num_rndrs-1] = s;
		rndr_list[num_rndrs] = NULL;
	}

	void		add_post_rndr(Renderer *s) {
		num_post_rndrs++;
		post_rndr_list = (Renderer **)realloc((void *)post_rndr_list, (num_post_rndrs+1) * sizeof(Renderer *));
		post_rndr_list[num_post_rndrs-1] = s;
		post_rndr_list[num_post_rndrs] = NULL;
	}

	void		add_scene(Scene *s) {
		num_scenes++;
		scene_list = (Scene **)realloc((void *)scene_list, (num_scenes+1) * sizeof(Scene *));
		scene_list[num_scenes-1] = s;
		scene_list[num_scenes] = NULL;
	}

	void		add_info(ImgInfo *s) {
		num_infos++;
		info_list = (ImgInfo **)realloc((void *)info_list, (num_infos+1) * sizeof(ImgInfo *));
		info_list[num_infos-1] = s;
		info_list[num_infos] = NULL;
	}

	void		cleanup(void) {
		Renderer **tlist;
		if(rndr_list) {
			for(tlist=rndr_list; *tlist; tlist++) {
				delete(*tlist);
			}
			free(rndr_list);
		}
		if(post_rndr_list) {
			for(tlist=post_rndr_list; *tlist; tlist++) {
				delete(*tlist);
			}
			free(post_rndr_list);
		}
		Scene **slist;
		if(scene_list) {
			for(slist=scene_list; *slist; slist++) {
				delete(*slist);
			}
			free(scene_list);
		}
		rndr_list = NULL;
		num_rndrs = 0;
		post_rndr_list = NULL;
		num_post_rndrs = 0;
		scene_list = NULL;
		num_scenes = 0;
		cleanup_infos();
	}

	void		cleanup_infos(void) {
		ImgInfo **ilist;
		if(info_list) {
			for(ilist=info_list; *ilist; ilist++) {
				delete(*ilist);
			}
			free(info_list);
		}
		info_list = NULL;
		num_infos = 0;
	}

   public:

	int set_scene(Scene *s, ImgInfo * =NULL);

	int render();

	int render(ObjNode *, ImgInfo *);

//	int render_start() { return(render()); }
   
//	int render_done() { return(TRUE); }

	int object_limit(void) { return(0); }

	MetaRenderer(void) {
		post_rndr_list = NULL;
		num_post_rndrs = 0;
		rndr_list = NULL;
		num_rndrs = 0;
		scene_list = NULL;
		num_scenes = 0;
		info_list = NULL;
		num_infos = 0;
	}
	~MetaRenderer(void) {
		cleanup();
	}

   }; /* Class */

#endif
