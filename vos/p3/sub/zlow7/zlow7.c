#include "xvmaininc.h"
#include "ftnbridge.h"
#include "ftnname.h"
#if 0
#include "low7.h"
#endif

int low7();

void zlow7(c0, c1, c2, c2a, c2b, c2c, c2d, c3, c3a, c3b, c4)
   struct card0   *c0;
   struct card1   *c1;
   struct card2   *c2;
   struct card2a  *c2a;
   struct card2b  *c2b;
   struct card2c  *c2c;
   struct card2d  *c2d;
   struct card3   *c3;
   struct card3a  *c3a;
   struct card3b  *c3b;
   struct card4   *c4;
{

/* NOTE:  This does not work.  The low7.h include file is missing, as */
/* is low7() itself.  Therefore I am simply commenting out the entire */
/* routine.  If it is ever resurrected, this can be restored. rgd 3/2010 */

#if 0
   FTN_NAME(low7)(
      c0->outfile, c0->tabfile,&c0->h2ofac,&c0->co2fac,&c0->o3fac,&c0->o2fac,
      &c0->ch4fac,&c0->so2fac, c0->rbuf, c0->tbuf, c0->sbuf,
      &c1->model,&c1->itype,&c1->iemsct,&c1->imult,&c1->m1,&c1->m2,&c1->m3,
      &c1->m4,&c1->m5,&c1->m6,&c1->mdef,&c1->im,&c1->noprt,&c1->tbound,
      c1->specalb, 
      &c2->ihaze,&c2->iseasn,&c2->ivulcn,&c2->icstl,&c2->icld,&c2->ivsa,
      &c2->vis,&c2->wss,&c2->whh,&c2->rainrt,&c2->gndalt, 
      &c3->h1, &c3->h2, &c3->angle, &c3->range, &c3->beta, &c3->ro, &c3->len, 
      &c4->v1, &c4->v2, &c4->dv, 
      &c2a->cthik, &c2a->calt, &c2a->cext, &c2a->iseed, 
      &c2b->zcvsa, &c2b->ztvsa, &c2b->zinvsa,
      &c2c->ml,&c2c->ird1,&c2c->ird2, c2c->title, c2c->zmdl, c2c->p, c2c->tx,
      c2c->wmol, c2c->jchar, c2c->ahaze, c2c->eqlwcz, c2c->rratz, c2c->iha1,
      c2c->icld1, c2c->ivul1, c2c->isea1, c2c->ichr1,
      c2d->ireg,c2d->awccon,c2d->title,c2d->vx,c2d->extc,c2d->absc, c2d->asym,
      &c3a->iparm,&c3a->iph,&c3a->iday,&c3a->isourc,&c3a->parm1,&c3a->parm2,
      &c3a->parm3,&c3a->parm4,&c3a->time,&c3a->psipo,&c3a->anglem,&c3a->g,
      &c3b->nangls, c3b->angf, c3b->f);
#endif

}
