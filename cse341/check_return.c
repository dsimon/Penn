void check_returns(struct nodetype *parse_tree)
{
  struct return_type *ret;

  if (parse_tree == NULL)
    return;

  if (parse_tree->nodeval == RETURN)
    {
      if (parse_tree->left == NULL)
	if (funct_return_type != VOID)
	  fprintf(stderr, "Function should return a value\n");
        else
	  return;
      ret = parse_expression(parse_tree->left);

      if (ret->rttype != funct_return_type) 
	  fprintf(stderr, "Function declaration and return value mismatch\n");
      free(ret);
      return;
    }
  check_returns(parse_tree->left);
  check_returns(parse_tree->right);
}
	
