#include "filter_expr_node.hpp"
#include "unary_arithmetic_filter.hpp"
#include "binary_arithmetic_filter.hpp"
#include "ternary_arithmetic_filter.hpp"
#include "field.hpp"

namespace xios
{
  CFilterFieldExprNode::CFilterFieldExprNode(const std::string& fieldId)
    : fieldId(fieldId)
  { /* Nothing to do */ }

  boost::shared_ptr<COutputPin> CFilterFieldExprNode::reduce(CGarbageCollector& gc, CField& thisField) const
  {
    boost::shared_ptr<COutputPin> outputPin;

    if (fieldId == "this")
      outputPin = thisField.getSelfReference(gc);
    else if (CField::has(fieldId))
    {
      CField* field = CField::get(fieldId);
      if (field == &thisField)
        ERROR("boost::shared_ptr<COutputPin> CFilterFieldExprNode::reduce(CGarbageCollector& gc, CField& thisField) const",
              << "The field " << fieldId << " has an invalid reference to itself. "
              << "Use the keyword \"this\" if you want to reference the input data sent to this field.");

      field->buildFilterGraph(gc, false);
      outputPin = field->getInstantDataFilter();
    }
    else
      ERROR("boost::shared_ptr<COutputPin> CFilterFieldExprNode::reduce(CGarbageCollector& gc, CField& thisField) const",
            << "The field " << fieldId << " does not exist.");

    return outputPin;
  }

  CFilterTemporalFieldExprNode::CFilterTemporalFieldExprNode(const std::string& fieldId)
    : fieldId(fieldId)
  { /* Nothing to do */ }

  boost::shared_ptr<COutputPin> CFilterTemporalFieldExprNode::reduce(CGarbageCollector& gc, CField& thisField) const
  {
    boost::shared_ptr<COutputPin> outputPin;

    if (fieldId == "this")
      outputPin = thisField.getSelfTemporalDataFilter(gc, thisField.freq_op.isEmpty() ? TimeStep : thisField.freq_op);
    else if (CField::has(fieldId))
    {
      CField* field = CField::get(fieldId);
      if (field == &thisField)
        ERROR("boost::shared_ptr<COutputPin> CFilterTemporalFieldExprNode::reduce(CGarbageCollector& gc, CField& thisField) const",
              << "The field " << fieldId << " has an invalid reference to itself. "
              << "Use the keyword \"this\" if you want to reference the input data sent to this field.");

      field->buildFilterGraph(gc, false);
      outputPin = field->getTemporalDataFilter(gc, thisField.freq_op.isEmpty() ? TimeStep : thisField.freq_op);
    }
    else
      ERROR("boost::shared_ptr<COutputPin> CFilterTemporalFieldExprNode::reduce(CGarbageCollector& gc, CField& thisField) const",
            << "The field " << fieldId << " does not exist.");

    return outputPin;
  }

  CFilterUnaryOpExprNode::CFilterUnaryOpExprNode(const std::string& opId, IFilterExprNode* child)
    : opId(opId)
    , child(child)
  {
    if (!child)
      ERROR("CFilterUnaryOpExprNode::CFilterUnaryOpExprNode(const std::string& opId, IFilterExprNode* child)",
            "Impossible to create the new expression node, an invalid child node was provided.");
  }

  boost::shared_ptr<COutputPin> CFilterUnaryOpExprNode::reduce(CGarbageCollector& gc, CField& thisField) const
  {
    boost::shared_ptr<CUnaryArithmeticFilter> filter(new CUnaryArithmeticFilter(gc, opId));
    child->reduce(gc, thisField)->connectOutput(filter, 0);
    return filter;
  }

  CFilterScalarFieldOpExprNode::CFilterScalarFieldOpExprNode(IScalarExprNode* child1, const std::string& opId, IFilterExprNode* child2)
    : child1(child1)
    , opId(opId)
    , child2(child2)
  {
    if (!child1 || !child2)
      ERROR("CFilterScalarFieldOpExprNode::CFilterScalarFieldOpExprNode(IScalarExprNode* child1, const std::string& opId, IFilterExprNode* child2)",
            "Impossible to create the new expression node, an invalid child node was provided.");
  }

  boost::shared_ptr<COutputPin> CFilterScalarFieldOpExprNode::reduce(CGarbageCollector& gc, CField& thisField) const
  {
    boost::shared_ptr<CScalarFieldArithmeticFilter> filter(new CScalarFieldArithmeticFilter(gc, opId, child1->reduce()));
    child2->reduce(gc, thisField)->connectOutput(filter, 0);
    return filter;
  }

  CFilterFieldScalarOpExprNode::CFilterFieldScalarOpExprNode(IFilterExprNode* child1, const std::string& opId, IScalarExprNode* child2)
    : child1(child1)
    , opId(opId)
    , child2(child2)
  {
    if (!child1 || !child2)
      ERROR("CFilterFieldScalarOpExprNode::CFilterFieldScalarOpExprNode(IFilterExprNode* child1, const std::string& opId, IScalarExprNode* child2)",
            "Impossible to create the new expression node, an invalid child node was provided.");
  }

  boost::shared_ptr<COutputPin> CFilterFieldScalarOpExprNode::reduce(CGarbageCollector& gc, CField& thisField) const
  {
    boost::shared_ptr<CFieldScalarArithmeticFilter> filter(new CFieldScalarArithmeticFilter(gc, opId, child2->reduce()));
    child1->reduce(gc, thisField)->connectOutput(filter, 0);
    return filter;
  }

  CFilterFieldFieldOpExprNode::CFilterFieldFieldOpExprNode(IFilterExprNode* child1, const std::string& opId, IFilterExprNode* child2)
    : child1(child1)
    , opId(opId)
    , child2(child2)
  {
    if (!child1 || !child2)
      ERROR("CFilterFieldFieldOpExprNode::CFilterFieldFieldOpExprNode(IFilterExprNode* child1, const std::string& opId, IFilterExprNode* child2)",
            "Impossible to create the new expression node, an invalid child node was provided.");
  }

  boost::shared_ptr<COutputPin> CFilterFieldFieldOpExprNode::reduce(CGarbageCollector& gc, CField& thisField) const
  {
    boost::shared_ptr<CFieldFieldArithmeticFilter> filter(new CFieldFieldArithmeticFilter(gc, opId));
    child1->reduce(gc, thisField)->connectOutput(filter, 0);
    child2->reduce(gc, thisField)->connectOutput(filter, 1);
    return filter;
  }




  CFilterScalarScalarFieldOpExprNode::CFilterScalarScalarFieldOpExprNode(IScalarExprNode* child1, const std::string& opId, IScalarExprNode* child2, IFilterExprNode* child3)
    : child1(child1)
    , opId(opId)
    , child2(child2)
    , child3(child3)
  {
    if (!child1 || !child2 || !child3)
      ERROR("  CFilterScalarScalarFieldOpExprNode::CFilterScalarScalarFieldOpExprNode(IScalarExprNode* child1, const std::string& opId, IScalarExprNode* child2, IFilterExprNode* child3)",
            "Impossible to create the new expression node, an invalid child node was provided.");
  }

  boost::shared_ptr<COutputPin> CFilterScalarScalarFieldOpExprNode::reduce(CGarbageCollector& gc, CField& thisField) const
  {
    boost::shared_ptr<CScalarScalarFieldArithmeticFilter> filter(new CScalarScalarFieldArithmeticFilter(gc, opId, child1->reduce(),child2->reduce()));
    child3->reduce(gc, thisField)->connectOutput(filter, 0);
    return filter;
  }


  CFilterScalarFieldScalarOpExprNode::CFilterScalarFieldScalarOpExprNode(IScalarExprNode* child1, const std::string& opId, IFilterExprNode* child2, IScalarExprNode* child3)
    : child1(child1)
    , opId(opId)
    , child2(child2)
    , child3(child3)
  {
    if (!child1 || !child2 || !child3)
      ERROR("  CFilterScalarFieldScalarOpExprNode::CFilterScalarFieldScalarOpExprNode(IScalarExprNode* child1, const std::string& opId, IFilterExprNode* child2, IScalarExprNode* child3)",
            "Impossible to create the new expression node, an invalid child node was provided.");
  }

  boost::shared_ptr<COutputPin> CFilterScalarFieldScalarOpExprNode::reduce(CGarbageCollector& gc, CField& thisField) const
  {
    boost::shared_ptr<CScalarFieldScalarArithmeticFilter> filter(new CScalarFieldScalarArithmeticFilter(gc, opId, child1->reduce(),child3->reduce()));
    child2->reduce(gc, thisField)->connectOutput(filter, 0);
    return filter;
  }


  CFilterScalarFieldFieldOpExprNode::CFilterScalarFieldFieldOpExprNode(IScalarExprNode* child1, const std::string& opId, IFilterExprNode* child2, IFilterExprNode* child3)
    : child1(child1)
    , opId(opId)
    , child2(child2)
    , child3(child3)
  {
    if (!child1 || !child2 || !child3)
      ERROR("  CFilterScalarFieldFieldOpExprNode::CFilterScalarFieldFieldOpExprNode(IScalarExprNode* child1, const std::string& opId, IFilterExprNode* child2, IFilterExprNode* child3)",
            "Impossible to create the new expression node, an invalid child node was provided.");
  }

  boost::shared_ptr<COutputPin> CFilterScalarFieldFieldOpExprNode::reduce(CGarbageCollector& gc, CField& thisField) const
  {
    boost::shared_ptr<CScalarFieldFieldArithmeticFilter> filter(new CScalarFieldFieldArithmeticFilter(gc, opId, child1->reduce()));
    child2->reduce(gc, thisField)->connectOutput(filter, 0);
    child3->reduce(gc, thisField)->connectOutput(filter, 1);
    return filter;
  }



  CFilterFieldScalarScalarOpExprNode::CFilterFieldScalarScalarOpExprNode(IFilterExprNode* child1, const std::string& opId, IScalarExprNode* child2, IScalarExprNode* child3)
    : child1(child1)
    , opId(opId)
    , child2(child2)
    , child3(child3)
  {
    if (!child1 || !child2 || !child3)
      ERROR("  CFilterFieldScalarScalarOpExprNode::CFilterFieldScalarScalarOpExprNode(IFilterExprNode* child1, const std::string& opId, IScalarExprNode* child2, IScalarExprNode* child3)",
            "Impossible to create the new expression node, an invalid child node was provided.");
  }

  boost::shared_ptr<COutputPin> CFilterFieldScalarScalarOpExprNode::reduce(CGarbageCollector& gc, CField& thisField) const
  {
    boost::shared_ptr<CFieldScalarScalarArithmeticFilter> filter(new CFieldScalarScalarArithmeticFilter(gc, opId, child2->reduce(),child3->reduce()));
    child1->reduce(gc, thisField)->connectOutput(filter, 0);
    return filter;
  }



  CFilterFieldScalarFieldOpExprNode::CFilterFieldScalarFieldOpExprNode(IFilterExprNode* child1, const std::string& opId, IScalarExprNode* child2, IFilterExprNode* child3)
    : child1(child1)
    , opId(opId)
    , child2(child2)
    , child3(child3)
  {
    if (!child1 || !child2 || !child3)
      ERROR("  CFilterFieldScalarFieldOpExprNode::CFilterFieldScalarFieldOpExprNode(IFilterExprNode* child1, const std::string& opId, IScalarExprNode* child2, IFilterExprNode* child3)",
            "Impossible to create the new expression node, an invalid child node was provided.");
  }

  boost::shared_ptr<COutputPin> CFilterFieldScalarFieldOpExprNode::reduce(CGarbageCollector& gc, CField& thisField) const
  {
    boost::shared_ptr<CFieldScalarFieldArithmeticFilter> filter(new CFieldScalarFieldArithmeticFilter(gc, opId, child2->reduce()));
    child1->reduce(gc, thisField)->connectOutput(filter, 0);
    child3->reduce(gc, thisField)->connectOutput(filter, 1);
    return filter;
  }



  CFilterFieldFieldScalarOpExprNode::CFilterFieldFieldScalarOpExprNode(IFilterExprNode* child1, const std::string& opId, IFilterExprNode* child2, IScalarExprNode* child3)
    : child1(child1)
    , opId(opId)
    , child2(child2)
    , child3(child3)
  {
    if (!child1 || !child2 || !child3)
      ERROR("  CFilterFieldFieldScalarOpExprNode::CFilterFieldFieldScalarOpExprNode(IFilterExprNode* child1, const std::string& opId, IFilterExprNode* child2, IScalarExprNode* child3)",
            "Impossible to create the new expression node, an invalid child node was provided.");
  }

  boost::shared_ptr<COutputPin> CFilterFieldFieldScalarOpExprNode::reduce(CGarbageCollector& gc, CField& thisField) const
  {
    boost::shared_ptr<CFieldFieldScalarArithmeticFilter> filter(new CFieldFieldScalarArithmeticFilter(gc, opId, child3->reduce()));
    child1->reduce(gc, thisField)->connectOutput(filter, 0);
    child2->reduce(gc, thisField)->connectOutput(filter, 1);
    return filter;
  }


  CFilterFieldFieldFieldOpExprNode::CFilterFieldFieldFieldOpExprNode(IFilterExprNode* child1, const std::string& opId, IFilterExprNode* child2, IFilterExprNode* child3)
    : child1(child1)
    , opId(opId)
    , child2(child2)
    , child3(child3)
  {
    if (!child1 || !child2 || !child3)
      ERROR("  CFilterFieldFieldFieldOpExprNode::CFilterFieldFieldFieldOpExprNode(IFilterExprNode* child1, const std::string& opId, IFilterExprNode* child2, IFilterExprNode* child3)",
            "Impossible to create the new expression node, an invalid child node was provided.");
  }

  boost::shared_ptr<COutputPin> CFilterFieldFieldFieldOpExprNode::reduce(CGarbageCollector& gc, CField& thisField) const
  {
    boost::shared_ptr<CFieldFieldFieldArithmeticFilter> filter(new CFieldFieldFieldArithmeticFilter(gc, opId));
    child1->reduce(gc, thisField)->connectOutput(filter, 0);
    child2->reduce(gc, thisField)->connectOutput(filter, 1);
    child3->reduce(gc, thisField)->connectOutput(filter, 2);
    return filter;
  }
  
}
