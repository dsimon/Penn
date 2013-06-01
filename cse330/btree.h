struct btree_type 
{
  struct btree_type *parent;
  struct btree_type *left;
  struct btree_type *middle;
  struct btree_type *middleright;
  struct btree_type *right;
  int leftdata;
  int middledata;
  int rightdata;
};

#ifdef MAIN
struct btree_type *tree_head;
int comps;
#else
extern struct btree_type *tree_head;
extern int comps;
#endif

#define NOVALUE        -1
#define FOUND          -2

#define TRUE           1
#define FALSE          0

