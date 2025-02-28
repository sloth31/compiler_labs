#include "FuncInfo.hpp"
#include "Function.hpp"

void FuncInfo::run() {
    for (auto &f : m_->get_functions()) {
        auto func = &f;
        trivial_mark(func);
        if (not is_pure[func])
            worklist.push_back(func);
    }
    while (worklist.empty() == false) {
        auto now = worklist.front();
        worklist.pop_front();
        process(now);
    }
    log();
}

void FuncInfo::log() {
    for (auto it : is_pure) {
        LOG_INFO << it.first->get_name() << " is pure? " << it.second;
    }
}

// 有 store 操作的函数非纯函数来处理
void FuncInfo::trivial_mark(Function *func) {
    if (func->is_declaration() or func->get_name() == "main") {//main函数一般为非纯函数
        is_pure[func] = false;
        return;
    }
    // 只要传入数组，都作为非纯函数处理
    for (auto it = func->get_function_type()->param_begin();
         it != func->get_function_type()->param_end(); ++it) {
        auto arg_type = *it;
        if (arg_type->is_integer_type() == false and
            arg_type->is_float_type() == false) {
            is_pure[func] = false;
            return;
        }
    }
    for (auto &bb : func->get_basic_blocks())
        for (auto &inst : bb.get_instructions()) {
            if (is_side_effect_inst(&inst)) {
                is_pure[func] = false;
                return;
            }
        }
    is_pure[func] = true;
}

void FuncInfo::process(Function *func) {
    for (auto &use : func->get_use_list()) {
        LOG_INFO << use.val_->print() << " uses func: " << func->get_name();
        if (auto inst = dynamic_cast<Instruction *>(use.val_)) {
            auto func = (inst->get_parent()->get_parent());
            if (is_pure[func]) {
                is_pure[func] = false;
                worklist.push_back(func);
            }
        } else
            LOG_WARNING << "Value besides instruction uses a function";
    }
}

// 对局部变量进行 store 没有副作用
bool FuncInfo::is_side_effect_inst(Instruction *inst) {
    if (inst->is_store()) {
        if (is_local_store(dynamic_cast<StoreInst *>(inst)))
            return false;
        return true;
    }
    if (inst->is_load()) {
        if (is_local_load(dynamic_cast<LoadInst *>(inst)))
            return false;
        return true;
    }
    // call 指令的副作用会在后续 bfs 中计算
    return false;
}

bool FuncInfo::is_local_load(LoadInst *inst) {
    auto addr =
        dynamic_cast<Instruction *>(get_first_addr(inst->get_operand(0)));
    if (addr and addr->is_alloca())
        return true;
    return false;
}

bool FuncInfo::is_local_store(StoreInst *inst) {
    auto addr = dynamic_cast<Instruction *>(get_first_addr(inst->get_lval()));
    if (addr and addr->is_alloca())
        return true;
    return false;
}
Value *FuncInfo::get_first_addr(Value *val) {
    if (auto inst = dynamic_cast<Instruction *>(val)) {
        if (inst->is_alloca())
            return inst;
        if (inst->is_gep())
            return get_first_addr(inst->get_operand(0));
        if (inst->is_load())
            return val;
        LOG_WARNING << "FuncInfo: try to determine addr in operands";
        for (auto op : inst->get_operands()) {
            if (op->get_type()->is_pointer_type())
                return get_first_addr(op);
        }
    }
    return val;
}
