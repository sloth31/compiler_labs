#include "cminusf_builder.hpp"

#define CONST_FP(num) ConstantFP::get((float)num, module.get())
#define CONST_INT(num) ConstantInt::get(num, module.get())
#define CREATE_BB_LABEL() snprintf(curBBLabel,sizeof(curBBLabel), "%08x", context.label++) //创建一个新基本块

// types
Type *VOID_T;
Type *INT1_T;
Type *INT32_T;
Type *INT32PTR_T;
Type *FLOAT_T;
Type *FLOATPTR_T;
char curBBLabel[32];//存储当前最新创建的基本块的label
/*
 * use CMinusfBuilder::Scope to construct scopes
 * scope.enter: enter a new scope
 * scope.exit: exit current scope
 * scope.push: add a new binding to current scope
 * scope.find: find and return the value bound to the name
 */

Value* CminusfBuilder::visit(ASTProgram &node) {
    VOID_T = module->get_void_type();
    INT1_T = module->get_int1_type();
    INT32_T = module->get_int32_type();
    INT32PTR_T = module->get_int32_ptr_type();
    FLOAT_T = module->get_float_type();
    FLOATPTR_T = module->get_float_ptr_type();

    Value *ret_val = nullptr;
    for (auto &decl : node.declarations) {
        ret_val = decl->accept(*this);
    }
    return ret_val;
}

Value* CminusfBuilder::visit(ASTNum &node) {//Num节点，都是右值
   if (node.type == TYPE_INT) {
        context.value = CONST_INT(node.i_val);
        context.is_lvalue = 0;
   }
   else if (node.type == TYPE_FLOAT)
   {
      context.value = CONST_FP(node.f_val);
      context.is_lvalue = 0;
   }
    return nullptr;
}

Value* CminusfBuilder::visit(ASTVarDeclaration &node) {//变量定义节点
   Type *vType;
   if (node.num == nullptr) // 定义变量而非数组， int a;
   {
      if(node.type==TYPE_FLOAT)
         vType = FLOAT_T;
      else
         vType = INT32_T;
   }else {//定义数组
      if(node.type==TYPE_INT)//整型数组
         vType=ArrayType::get(INT32_T, node.num->i_val);
      else  
         vType=ArrayType::get(FLOAT_T, node.num->i_val);
   }
   //处理作用域
   Value *varValue;
   if (scope.in_global()) { //声明的是全局变量
        varValue = GlobalVariable::create(node.id, module.get(), vType, false, ConstantZero::get(vType, module.get()));
    } else {
        varValue = builder->create_alloca(vType);//为新变量分配空间
    }
    scope.push(node.id, varValue);
    return nullptr;
}

Value* CminusfBuilder::visit(ASTFunDeclaration &node) {
    FunctionType *fun_type;//函数类型
    Type *ret_type;//返回类型
    std::vector<Type *> paramTypes;//函数参数列表
    if (node.type == TYPE_INT)
        ret_type = INT32_T;
    else if (node.type == TYPE_FLOAT)
        ret_type = FLOAT_T;
    else
        ret_type = VOID_T;

    for (auto &param : node.params) {
        if (param->isarray) {//数组类型参数
            if (param->type == TYPE_INT)
                paramTypes.push_back(INT32PTR_T);
            else
                paramTypes.push_back(FLOATPTR_T);
        } else {
            if (param->type == TYPE_INT)
                paramTypes.push_back(INT32_T);
            else
                paramTypes.push_back(FLOAT_T);
        }
    }

    fun_type = FunctionType::get(ret_type, paramTypes);
    auto func = Function::create(fun_type, node.id, module.get());
    scope.push(node.id, func);//加入作用域中的符号表
    context.func = func;
    auto funBB = BasicBlock::create(module.get(), "entry", func);
    builder->set_insert_point(funBB);//进入函数所在基本块
    scope.enter();
    std::vector<Value *> args;
    for (auto &arg : func->get_args()) {//获取形参
        args.push_back(&arg);
    }
    for (int i = 0; i < node.params.size(); ++i) {
        Type *argType=nullptr;//参数类型
        int argTypeFlag = ((node.params[i]->type == TYPE_INT) << 1) + node.params[i]->isarray;
        switch (argTypeFlag) {
            case 0b00:
                argType = FLOAT_T; break;
            case 0b01:
                argType = FLOATPTR_T; break;
            case 0b10:
                argType = INT32_T; break;
            case 0b11:
                argType = INT32PTR_T; break;
            default: break;
        }
        auto argAlloca = builder->create_alloca(argType);
        builder->create_store(args[i], argAlloca);
        scope.push(node.params[i]->id, argAlloca);//维护符号表，把声明的形参也加入
    }
    node.compound_stmt->accept(*this);
    if (not builder->get_insert_block()->is_terminated())
    {
        if (context.func->get_return_type()->is_void_type())
            builder->create_void_ret();
        else if (context.func->get_return_type()->is_float_type())
            builder->create_ret(CONST_FP(0.));
        else
            builder->create_ret(CONST_INT(0));
    }
    scope.exit();
    return nullptr;
}

Value* CminusfBuilder::visit(ASTParam &node) {
    // 已经在ASTFunDeclaration中处理
    return nullptr;
}

Value* CminusfBuilder::visit(ASTCompoundStmt &node) {//{}括起来的语句，相当于一个独立的作用域
    // You may need to add some code here
    // to deal with complex statements.
    scope.enter();
    for (auto &decl : node.local_declarations)
    {
       decl->accept(*this);
    }

    for (auto &stmt : node.statement_list) {
        stmt->accept(*this);
        if (builder->get_insert_block()->is_terminated())
            break;
    }
    scope.exit();
    return nullptr;
}

Value* CminusfBuilder::visit(ASTExpressionStmt &node) {
    if (node.expression != nullptr) {
        node.expression->accept(*this);
    }//委托给expression递归处理
    return nullptr;
}

Value* CminusfBuilder::visit(ASTSelectionStmt &node) {//if else语句
   CREATE_BB_LABEL();
   auto trueBB = BasicBlock::create(module.get(), curBBLabel, context.func);
   CREATE_BB_LABEL();
   auto falseBB = BasicBlock::create(module.get(), curBBLabel, context.func);
   CREATE_BB_LABEL();
   auto nextBB = BasicBlock::create(module.get(), curBBLabel, context.func);
   node.expression->accept(*this);//处理掉if判断条件expression并获取返回值（存储在context.value中）
   if (context.value->get_type()->is_integer_type()) {//判断expression返回值是否为int
        context.value = builder->create_icmp_ne(context.value, CONST_INT(0));
    } else {
        context.value = builder->create_fcmp_ne(context.value, CONST_FP(0.));
    }
    builder->create_cond_br(context.value, trueBB, falseBB);
   //true分支
   builder->set_insert_point(trueBB);
    scope.enter();
    node.if_statement->accept(*this);//处理if后跟的复合语句({}括起来的部分)
    scope.exit();
    if (!builder->get_insert_block()->is_terminated()) {//如果没有显式的终止指令，另外添加跳转指令
        builder->create_br(nextBB);
    }
    //false分支
   builder->set_insert_point(falseBB);
    if (node.else_statement != nullptr) {//有else分支的情况
        scope.enter();
        node.else_statement->accept(*this);
        scope.exit();
        if (!builder->get_insert_block()->is_terminated()) {
            builder->create_br(nextBB);
        }
    } else {
        builder->create_br(nextBB);
    }
    builder->set_insert_point(nextBB);
   return nullptr;
}

Value* CminusfBuilder::visit(ASTIterationStmt &node) {
    //while语句
    CREATE_BB_LABEL();
    auto condBB = BasicBlock::create(module.get(), curBBLabel, context.func);//while的判断语句基本块
    CREATE_BB_LABEL();
    auto loopBodyBB = BasicBlock::create(module.get(), curBBLabel, context.func);//循环体基本块
    CREATE_BB_LABEL();
    auto nextBB = BasicBlock::create(module.get(), curBBLabel, context.func);//循环结束后跳转的基本块
    if (!builder->get_insert_block()->is_terminated()) {
        builder->create_br(condBB);
    }
      builder->set_insert_point(condBB);
    node.expression->accept(*this);
    if (context.value->get_type()->is_integer_type()) {
        context.value = builder->create_icmp_ne(context.value, CONST_INT(0));
    } else {
        context.value = builder->create_fcmp_ne(context.value, CONST_FP(0.));
    }

    builder->create_cond_br(context.value, loopBodyBB, nextBB);
    builder->set_insert_point(loopBodyBB);
    scope.enter();
    node.statement->accept(*this);
    scope.exit();
    if (!builder->get_insert_block()->is_terminated()) {
        builder->create_br(condBB);
    }
    builder->set_insert_point(nextBB);//跳转到下一个基本块

    return nullptr;
}

Value* CminusfBuilder::visit(ASTReturnStmt &node) {//return语句
    if (node.expression == nullptr) {
        builder->create_void_ret();
        return nullptr;
    } else {
        node.expression->accept(*this);
        auto retType = context.func->get_function_type()->get_return_type();

        if (retType != context.value->get_type()) { // 强制类型转换
            if (context.value->get_type()->is_integer_type()) {
                context.value = builder->create_sitofp(context.value, FLOAT_T);
            } else {
                context.value = builder->create_fptosi(context.value, INT32_T);
            }
        }
        builder->create_ret(context.value);
    }
    return nullptr;
}

Value* CminusfBuilder::visit(ASTVar &node) {
    context.value = scope.find(node.id);
    if (node.expression == nullptr) {//单独的变量使用，没有类似数组取值访问的操作 e.g. a[1]
        if (context.is_lvalue == 0) {//右值，需要先load出来
            if (context.value->get_type()->get_pointer_element_type()->is_array_type()) {
                context.value = builder->create_gep(context.value, {CONST_INT(0), CONST_INT(0)});//取指针
            } else {
                context.value = builder->create_load(context.value);
            }
        }
    }else{// e.g. a[1] 需要考虑负索引、越界的情况 也需要考虑嵌套情况 a[b[1]]、a[index]、a[i=1]
      auto * array_Value = context.value;//数组名
      bool backup_is_lvalue = context.is_lvalue;
      context.is_lvalue = 0;//数组取值可能涉及load操作，此时先假设在处理右值
      node.expression->accept(*this);
      context.is_lvalue = backup_is_lvalue;
      auto *indexValue = context.value;//索引值
      if (indexValue->get_type()->is_float_type()) {
            indexValue = builder->create_fptosi(indexValue, INT32_T);//强制类型转换成int类型
        }
      CREATE_BB_LABEL();
      auto validBB = BasicBlock::create(module.get(), curBBLabel, context.func);//合法index
      CREATE_BB_LABEL();
      auto unvalidBB = BasicBlock::create(module.get(), curBBLabel, context.func);//不合法
      CREATE_BB_LABEL();
      auto nextBB = BasicBlock::create(module.get(), curBBLabel, context.func);//之后要跳转的基本块
      auto *condForIndex = builder->create_icmp_ge(indexValue, CONST_INT(0));//下标>=0
      builder->create_cond_br(condForIndex, validBB, unvalidBB);
      //valid
      builder->set_insert_point(validBB);
      if (array_Value->get_type()->get_pointer_element_type()->is_integer_type() || array_Value->get_type()->get_pointer_element_type()->is_float_type()) {
            context.value = builder->create_gep(array_Value, {indexValue});
        } else if (array_Value->get_type()->get_pointer_element_type()->is_pointer_type()) {
            array_Value = builder->create_load(array_Value);
            context.value = builder->create_gep(array_Value, {indexValue});
        } else {
            context.value = builder->create_gep(array_Value, {CONST_INT(0), indexValue});
        }
        if (context.is_lvalue == 0) {
            context.value = builder->create_load(context.value);
        }
        builder->create_br(nextBB);
        // unvalid
        builder->set_insert_point(unvalidBB);
        builder->create_call(scope.find("neg_idx_except"), {});//调用预定义函数neg_idx_except
        builder->create_br(nextBB);
        //next
        builder->set_insert_point(nextBB);
    }
    return nullptr;
}

Value* CminusfBuilder::visit(ASTAssignExpression &node) { //var = expression
    context.is_lvalue = true;//首先处理左值
    node.var->accept(*this);
    context.is_lvalue = false;

    auto _var = context.value;//var
    node.expression->accept(*this);
    auto varAlloca = context.value;//需要给var赋的值
    auto expType = varAlloca->get_type();//expression的type

    if (_var->get_type()->get_pointer_element_type() != expType) {
        if (expType->is_integer_type()) {//强制类型转换
            varAlloca = builder->create_sitofp(varAlloca, FLOAT_T);
        } else {
            varAlloca = builder->create_fptosi(varAlloca, INT32_T);
        }
    }
    builder->create_store(varAlloca, _var);
    context.value = varAlloca;
    return nullptr;
}

Value* CminusfBuilder::visit(ASTSimpleExpression &node) {//简单表达式，包含多种运算符
    node.additive_expression_l->accept(*this);
    if (node.additive_expression_r == nullptr) { // simple-expression→additive-expression 的情况
        return nullptr;
    }
    auto *lVal = context.value;
    node.additive_expression_r->accept(*this);
    auto *rVal = context.value;
    // 强制类型转换
    if (lVal->get_type()->is_float_type() || context.value->get_type()->is_float_type()) { // 存在float类型
        if (lVal->get_type()->is_integer_type()) {
            lVal = builder->create_sitofp(lVal, FLOAT_T);
        }
        if (rVal->get_type()->is_integer_type()) {
            rVal = builder->create_sitofp(rVal, FLOAT_T);
        }
        switch (node.op) {
            case OP_LT:
                context.value = builder->create_fcmp_lt(lVal, rVal); break;
            case OP_GT:
                context.value = builder->create_fcmp_gt(lVal, rVal); break;
            case OP_LE:
                context.value = builder->create_fcmp_le(lVal, rVal); break;
            case OP_GE:
                context.value = builder->create_fcmp_ge(lVal, rVal); break;
            case OP_EQ:
                context.value = builder->create_fcmp_eq(lVal, rVal); break;
            case OP_NEQ:
                context.value = builder->create_fcmp_ne(lVal, rVal); break;
        }
    } else {
        switch (node.op) {
            case OP_LT:
                context.value = builder->create_icmp_lt(lVal, rVal); break;
            case OP_GT:
                context.value = builder->create_icmp_gt(lVal, rVal); break;
            case OP_LE:
                context.value = builder->create_icmp_le(lVal, rVal); break;
            case OP_GE:
                context.value = builder->create_icmp_ge(lVal, rVal); break;
            case OP_EQ:
                context.value = builder->create_icmp_eq(lVal, rVal); break;
            case OP_NEQ:
                context.value = builder->create_icmp_ne(lVal, rVal); break;
        }
    }
    context.value = builder->create_zext(context.value, INT32_T);
    return nullptr;
}

Value* CminusfBuilder::visit(ASTAdditiveExpression &node) {
   if (node.additive_expression == nullptr) {//additive-expression -> term
        node.term->accept(*this);
    } else {
        node.additive_expression->accept(*this);
        auto *lVal = context.value;
        node.term->accept(*this);
        auto *rVal = context.value;

        if (lVal->get_type()->is_float_type() || context.value->get_type()->is_float_type()) { // 存在float类型
            if (lVal->get_type()->is_integer_type()) {
                lVal = builder->create_sitofp(lVal, FLOAT_T);
            }
            if (rVal->get_type()->is_integer_type()) {
                rVal = builder->create_sitofp(rVal, FLOAT_T);
            }
            if (node.op == OP_PLUS) {
                context.value = builder->create_fadd(lVal, rVal);
            } else if (node.op == OP_MINUS) {
                context.value = builder->create_fsub(lVal, rVal);
            }
        } else {
            if (node.op == OP_PLUS) {
                context.value = builder->create_iadd(lVal, rVal);
            } else if (node.op == OP_MINUS) {
                context.value = builder->create_isub(lVal, rVal);
            }
        }
    }
    return nullptr;
}

Value* CminusfBuilder::visit(ASTTerm &node) {
if (node.term == nullptr) {//term → factor
        node.factor->accept(*this);
        return nullptr;
    }
    node.term->accept(*this);
    auto *lVal = context.value;
    node.factor->accept(*this);
    auto *rVal = context.value;

    if (lVal->get_type()->is_float_type() || context.value->get_type()->is_float_type()) { //存在float
        if (lVal->get_type()->is_integer_type()) {
            lVal = builder->create_sitofp(lVal, FLOAT_T);
        }
        if (rVal->get_type()->is_integer_type()) {
            rVal = builder->create_sitofp(rVal, FLOAT_T);
        }
        if (node.op == OP_MUL) {
            context.value = builder->create_fmul(lVal, rVal);
        } else if (node.op == OP_DIV) {
            context.value = builder->create_fdiv(lVal, rVal);
        }
    } else {
        if (node.op == OP_MUL) {
            context.value = builder->create_imul(lVal, rVal);
        } else if (node.op == OP_DIV) {
            context.value = builder->create_isdiv(lVal, rVal);
        }
    }
    return nullptr;
}

Value* CminusfBuilder::visit(ASTCall &node) {
    auto *func = (Function *)(scope.find(node.id));
    auto param = func->get_function_type()->param_begin();
    std::vector<Value *> args;
    for (auto &arg : node.args) {
        arg->accept(*this);
        auto *vType = context.value->get_type();
        if (vType != *param && !vType->is_pointer_type()) {
            if (vType->is_integer_type()) {//强制类型转换
                context.value = builder->create_sitofp(context.value, *param);
            } else {
                context.value = builder->create_fptosi(context.value, *param);
            }
        }
        param++;
        args.push_back(context.value);
    }
    context.value = builder->create_call(func, args);
    return nullptr;
}
