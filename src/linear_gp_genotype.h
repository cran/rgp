/* linear_gp_genotype.h
 *
 */

#ifndef LINEAR_GP_GENOTYPE_H
#define LINEAR_GP_GENOTYPE_H

#include <R.h>
#include <Rinternals.h>


#define MAX_ARITY 3


typedef union Linear_GP_Constant {
  SEXP sexp_constant;
  double double_constant;
} Linear_GP_Constant;

typedef enum {
  ADD_OPCODE, SUB_OPCODE, MUL_OPCODE, DIV_OPCODE,
  EXP_OPCODE, LOG_OPCODE
} Linear_GP_FixedOpcode;

typedef struct Linear_GP_Operation {
  union {
    SEXP sexp_opcode;
    Linear_GP_FixedOpcode fixed_opcode;
  } opcode;
  int operands[MAX_ARITY];
} Linear_GP_Operation;

typedef struct Linear_GP_Node {
  enum { CONSTANT_NODE, OPERATION_NODE } tag;
  union {
    Linear_GP_Constant constant_node;
    Linear_GP_Operation operation_node;
  } node_data;
} Linear_GP_Node;


Linear_GP_Node linear_GP_SEXP_constant(const SEXP constant);
Linear_GP_Node linear_GP_double_constant(const double constant);
Linear_GP_Node linear_GP_SEXP_operation(const SEXP opcode, const int operands[MAX_ARITY]);
Linear_GP_Node linear_GP_fixed_operation(const Linear_GP_FixedOpcode opcode, const int operands[MAX_ARITY]);

void linear_GP_Node_set_operands(Linear_GP_Node *node, const int operands[MAX_ARITY]);

// TODO

#endif
