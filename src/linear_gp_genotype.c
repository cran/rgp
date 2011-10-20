/* linear_gp_genotype.c
 *
 */

#include "linear_gp_genotype.h"
#include <stdio.h>


Linear_GP_Node linear_GP_SEXP_constant(const SEXP constant) {
  Linear_GP_Node n = { .tag = CONSTANT_NODE,
                       .node_data = { .constant_node = { .sexp_constant = constant } }
  };
  return n;
}

Linear_GP_Node linear_GP_double_constant(const double constant) {
  Linear_GP_Node n = { .tag = CONSTANT_NODE,
                       .node_data = { .constant_node = { .double_constant = constant } }
  };
  return n;
}

Linear_GP_Node linear_GP_SEXP_operation(const SEXP opcode, const int operands[MAX_ARITY]) {
  Linear_GP_Node n = { .tag = OPERATION_NODE,
                       .node_data = { .operation_node = { .opcode = { .sexp_opcode = opcode } } }
  };
  linear_GP_Node_set_operands(&n, operands);
  return n;
}

Linear_GP_Node linear_GP_fixed_operation(const Linear_GP_FixedOpcode opcode, const int operands[MAX_ARITY]) {
  Linear_GP_Node n = { .tag = OPERATION_NODE,
                       .node_data = { .operation_node = { .opcode = { .fixed_opcode = opcode } } }
  };
  linear_GP_Node_set_operands(&n, operands);
  return n;
}

void linear_GP_Node_set_operands(Linear_GP_Node *node, const int operands[MAX_ARITY]) {
  // looping should be faster than memcpy(), as MAX_ARITY is small
  for (int i = 0; i < MAX_ARITY; i++) {
    node->node_data.operation_node.operands[i] = operands[i];
  }
}

// TODO
SEXP test_lgp() {
  Rprintf("Hello, this ist lgp!\n");
  Linear_GP_Node n0 = linear_GP_double_constant(3.14);
  const int ops0[MAX_ARITY] = { 1, 2, 3 };
  Linear_GP_Node n1 = linear_GP_fixed_operation(MUL_OPCODE, ops0);
  Linear_GP_Node gt0[2] = { n0, n1 };
  for (int i = 0; i < MAX_ARITY; i++) {
    Rprintf("%d\n", gt0[1].node_data.operation_node.operands[i]);
  }
  return R_NilValue;
}
