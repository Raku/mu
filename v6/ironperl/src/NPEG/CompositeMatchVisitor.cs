#region License
/* **********************************************************************************
 * Copyright (c) Leblanc Meneses
 * This source code is subject to terms and conditions of the MIT License
 * for NPEG. A copy of the license can be found in the License.txt file
 * at the root of this distribution. 
 * By using this source code in any fashion, you are agreeing to be bound by the terms of the 
 * MIT License.
 * You must not remove this notice from this software.
 * **********************************************************************************/
#endregion
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;

using NPEG.NonTerminals;
using NPEG.Terminals;

namespace NPEG
{
    public class CompositeMatchVisitor : ICompositeGrammarVisitor
    {
        Stack<IsMatchPredicate> matchStack = new Stack<IsMatchPredicate>();
        public delegate Boolean IsMatchPredicate(InputIterator iterator);


        protected InputIterator iterator;
        public Dictionary<Int32, String> Error = new Dictionary<int, string>();

        public CompositeMatchVisitor(InputIterator iterator)
            : base()
        {
            this.iterator = iterator;
        }

        public override void Dispose()
        {
        }

        public AstNode AST
        {
            get;
            set;
        }

        public Boolean IsMatch
        {
            get 
            {
                return matchStack.Peek()(this.iterator);
            }
        }



        #region nonterminals
        // nonterminal must save iterator position.
        public override void VisitEnter(AndPredicate expression)
        {
            this.disableCapturing.Push(true);
        }
        public override void VisitExecute(AndPredicate expression){}
        public override void VisitLeave(AndPredicate expression)
        {
            this.disableCapturing.Pop();

            IsMatchPredicate exp = this.matchStack.Pop();
            this.matchStack.Push(
                new IsMatchPredicate(
                    delegate(InputIterator iterator)
                    {
                        this.disableBackReferencePop.Push(true);
                        Boolean result = true;
                        Int32 savePosition = iterator.Index;
                        if (exp(iterator))
                        {
                            iterator.Index = savePosition;
                            result &= true;
                        }
                        else
                        {
                            iterator.Index = savePosition;
                            result &= false;
                        }

                        this.disableBackReferencePop.Pop();
                        return result;
                    }
                )
             );
        }

        public override void VisitEnter(NotPredicate expression)
        {
            this.disableCapturing.Push(true);
        }
        public override void VisitExecute(NotPredicate expression){}
        public override void VisitLeave(NotPredicate expression)
        {
            this.disableCapturing.Pop();

            IsMatchPredicate local = this.matchStack.Pop();
            this.matchStack.Push(
                new IsMatchPredicate(
                    delegate(InputIterator iterator)
                    {
                        this.disableBackReferencePop.Push(true);

                        Boolean result = true;
                        Int32 savePosition = iterator.Index;
                        if (!local(iterator))
                        {
                            iterator.Index = savePosition;
                            result &= true;
                        }
                        else
                        {
                            iterator.Index = savePosition;
                            result &= false;
                        }

                        this.disableBackReferencePop.Pop();
                        return result;
                    }
                )
             );
        }





        #warning create another thread and process these simultaneously.
        public override void VisitEnter(PrioritizedChoice expression){}
        public override void VisitExecute(PrioritizedChoice expression) { }
        public override void VisitLeave(PrioritizedChoice expression)
        {
            IsMatchPredicate localRight = this.matchStack.Pop();
            IsMatchPredicate localLeft = this.matchStack.Pop();
            this.matchStack.Push(
                new IsMatchPredicate(
                    delegate(InputIterator iterator)
                    {
                        Int32 savePosition = iterator.Index;

                        if (localLeft(iterator))
                        {
                            return true;
                        }

                        iterator.Index = savePosition;

                        if (localRight(iterator))
                        {
                            return true;
                        }

                        return false;
                    }
                )
             );
        }

        public override void VisitEnter(ZeroOrMore expression){}
        public override void VisitExecute(ZeroOrMore expression){}
        public override void VisitLeave(ZeroOrMore expression)
        {
            IsMatchPredicate local = this.matchStack.Pop();
            this.matchStack.Push(
                new IsMatchPredicate(
                    delegate(InputIterator iterator)
                    {
                        Int32 savePosition = iterator.Index;
                        while (local(iterator))
                        {
                            savePosition = iterator.Index;
                        }

                        iterator.Index = savePosition;
                        return true;
                    }
                )
             );
        }

        public override void VisitEnter(OneOrMore expression){}
        public override void VisitExecute(OneOrMore expression){}
        public override void VisitLeave(OneOrMore expression)
        {
            IsMatchPredicate local = this.matchStack.Pop();
            this.matchStack.Push(
                new IsMatchPredicate(
                    delegate(InputIterator iterator)
                    {
                        Int32 cnt = 0;
                        Int32 savePosition = iterator.Index;
                        while (local(iterator))
                        {
                            savePosition = iterator.Index;
                            cnt++;
                        }

                        iterator.Index = savePosition;

                        return (cnt > 0);
                    }
                )
             );
        }

        public override void VisitEnter(Optional expression){}
        public override void VisitExecute(Optional expression){}
        public override void VisitLeave(Optional expression)
        {
            IsMatchPredicate local = this.matchStack.Pop();
            this.matchStack.Push(
                new IsMatchPredicate(
                    delegate(InputIterator iterator)
                    {
                        Int32 savePosition = iterator.Index;
                        if (local(iterator))
                        {
                            savePosition = iterator.Index;
                        }
                        else
                        {
                            iterator.Index = savePosition;
                        }
                        return true;
                    }
                )
             );
        }


        public override void VisitEnter(LimitingRepetition expression){ }
        public override void VisitExecute(LimitingRepetition expression) { }
        public override void VisitLeave(LimitingRepetition expression)
        {
            Int32? max = expression.Max;
            Int32? min = expression.Min;
            IsMatchPredicate local = this.matchStack.Pop();

            this.matchStack.Push(
                new IsMatchPredicate(
                    delegate(InputIterator iterator)
                    {
                        Int32 cnt = 0;
                        Int32 savePosition = iterator.Index;
                        Boolean result = false;

                        if (min != null)
                        {
                            if (max == null)
                            {
                                // has a minimum but no max cap
                                savePosition = iterator.Index;
                                while (local(iterator))
                                {
                                    cnt++;
                                    savePosition = iterator.Index;
                                }

                                iterator.Index = savePosition;
                                result = (cnt >= min);
                            }
                            else
                            {
                                // has a minimum and a max specified

                                if (max < min)
                                {
                                    throw new ArgumentException("A Max property must be larger than Min when using LimitingRepetition.");
                                }

                                savePosition = iterator.Index;
                                while (local(iterator))
                                {
                                    cnt++;
                                    savePosition = iterator.Index;

                                    if (cnt >= max)
                                    {
                                        break;
                                    }
                                }

                                iterator.Index = savePosition;
                                result = (cnt <= max && cnt >= min);
                            }
                        }
                        else
                        {
                            if (max == null)
                            {
                                throw new ArgumentException("A Min and/or Max must be specified when using LimitingRepetition.");
                            }
                            else
                            {
                                // zero or up to a max matches of e.
                                savePosition = iterator.Index;
                                while (local(iterator))
                                {
                                    cnt++;
                                    savePosition = iterator.Index;

                                    if (cnt >= max)
                                    {
                                        break;
                                    }
                                }

                                iterator.Index = savePosition;
                                result = (cnt <= max);
                            }
                        }

                        return result;
                    }
                )
             );
        }


        public override void VisitEnter(Sequence expression){}
        public override void VisitExecute(Sequence expression){}
        public override void VisitLeave(Sequence expression)
        {
            IsMatchPredicate localRight = this.matchStack.Pop();
            IsMatchPredicate localLeft = this.matchStack.Pop();
            this.matchStack.Push(
                new IsMatchPredicate(
                    delegate(InputIterator iterator)
                    {
                        Boolean result = true;
                        Int32 savePosition = iterator.Index;
                        if (localLeft(iterator) && localRight(iterator))
                        {
                            result &= true;
                        }
                        else
                        {
                            iterator.Index = savePosition;
                            result &= false;
                        }
                        return result;
                    }
                )
             );
        }


        public override void VisitEnter(CapturingGroup expression) {}
        public override void VisitExecute(CapturingGroup expression) { }


        Dictionary<String, Stack<String>> backReferenceLookup = new Dictionary<string, Stack<String>>();
        Stack<Boolean> disableBackReferencePop = new Stack<bool>();
        Stack<Boolean> disableCapturing = new Stack<bool>();
            // turned on/off by predicates; processing a predicate if stack is larger than 0
        Stack<AstNode> parent = new Stack<AstNode>();
            // stack used to build AST.
        public override void VisitLeave(CapturingGroup expression)
        {
            if (disableCapturing.Count == 0)
                // no predicates being processed.
            {
                String name = expression.Name;
                Boolean reduceBySingleChildNode = expression.DoReplaceBySingleChildNode;
                IsMatchPredicate local = this.matchStack.Pop();

                IAstNodeReplacement createType = expression.ReplacementNode;

                this.matchStack.Push(
                    new IsMatchPredicate(
                        delegate(InputIterator iterator)
                        {
                            Boolean result = true;
                            Int32 savePosition = iterator.Index;

                            this.parent.Push(new AstNode());

                            if (local(iterator))
                            {
                                this.Error.Clear();

                                String matchedText = iterator.GetText(savePosition, iterator.Index);
                                if (this.backReferenceLookup.ContainsKey(name))
                                {
                                    this.backReferenceLookup[name].Push(matchedText);
                                }
                                else
                                {
                                    this.backReferenceLookup.Add(name, new Stack<string>());
                                    this.backReferenceLookup[name].Push(matchedText);
                                }


                                AstNode node = this.parent.Pop();
                                node.Token = new TokenMatch(name, matchedText,
                                                    savePosition, iterator.Index);


                                if (createType != null)
                                {
                                    // create a custom astnode
                                    IAstNodeReplacement nodevisitor = (IAstNodeReplacement)Activator.CreateInstance(createType.GetType(), new Object[] { });
                                    nodevisitor.Token = node.Token;
                                    nodevisitor.Parent = node.Parent;
                                    nodevisitor.Children = node.Children;
                                    foreach (AstNode updateparent in nodevisitor.Children)
                                    {
                                        updateparent.Parent = nodevisitor;
                                    }

                                    node.Accept(nodevisitor);
                                    node = nodevisitor;
                                }


                                if (reduceBySingleChildNode)
                                {
                                    if (node.Children.Count == 1)
                                    {
                                        node = node.Children[0];
                                    }
                                }



                                if (this.parent.Count > 0)
                                {
                                    node.Parent = this.parent.Peek();
                                    this.parent.Peek().Children.Add(node);
                                }
                                else
                                {
                                    this.AST = node;
                                }

                                savePosition = iterator.Index;
                                result &= true;
                            }
                            else
                            {
                                this.parent.Pop();
                                iterator.Index = savePosition;
                                result &= false;
                            }

                            return result;
                        }
                    )
                 );
            }
        }

        Dictionary<String, IsMatchPredicate> recursiveMethod = new Dictionary<string, IsMatchPredicate>();
        public override void VisitEnter(RecursionCreate expression) { }
        public override void VisitExecute(RecursionCreate expression) {}
        public override void VisitLeave(RecursionCreate expression) 
        {
            IsMatchPredicate local = this.matchStack.Pop();
            this.recursiveMethod.Add(
                expression.FunctionName
                ,
                new IsMatchPredicate(
                    delegate(InputIterator iterator)
                    {
                        return local(iterator);
                    }
                )
            );

            this.matchStack.Push(this.recursiveMethod[expression.FunctionName]);
        }
        #endregion



        #region terminals
        // terminals consume characters
        public override void Visit(Literal expression)
        {
            Boolean iscasesensitive = expression.IsCaseSensitive;
            String data = expression.MatchText;
            this.matchStack.Push(
                new IsMatchPredicate(
                    delegate(InputIterator iterator)
                    {
                        foreach(Char c in data)
                        {
                            if (iscasesensitive)
                            {
                                if (iterator.Current != c)
                                {
                                    iterator.Next();
                                    if (!this.Error.ContainsKey(iterator.Index))
                                        this.Error.Add(iterator.Index, "Literal: " + data);
                                    return false;
                                }
                            }
                            else
                            {
                                if (iterator.Current.ToString().ToUpper() != c.ToString().ToUpper())
                                {
                                    iterator.Next();

                                    if (!this.Error.ContainsKey(iterator.Index))
                                        this.Error.Add(iterator.Index, "Literal: " + data);
                                    return false;
                                }
                            }
                            iterator.Next();
                        }

                        return true;
                    }
                )
             );
        }

        public override void Visit(CodePoint expression)
        {
            String matchtext = expression.MatchText;
            CodePoint.ShiftDirectionType shiftdirection = expression.ShiftDirection;

            this.matchStack.Push(
                new IsMatchPredicate(
                    delegate(InputIterator iterator)
                    {
                        Boolean result = false;
                        if (matchtext.StartsWith("#x"))
                        {
                            //hexadecimal evaluation
                            switch (shiftdirection)
                            {
                                case CodePoint.ShiftDirectionType.Left:
                                    break;
                                case CodePoint.ShiftDirectionType.Right:
                                    break;
                                default:
                                    throw new NotImplementedException("CodePoint: ShiftDirection");
                            }
                        }
                        else if (matchtext.StartsWith("#b"))
                        {
                            // binary evaluation

                        }
                        else
                        {
                            // decimal evaluation
                        }
                        return result;
                    }
                )
             );


        }


        public override void Visit(AnyCharacter expression)
        {
            this.matchStack.Push(
                new IsMatchPredicate(
                    delegate(InputIterator iterator)
                    {
                        Boolean result = true;
                        iterator.Next();
                        if(iterator.Length == 0 || iterator.Length <= iterator.Index)
                        {
                            iterator.Previous();
                            result = false;

                            if (!this.Error.ContainsKey(iterator.Index))
                                this.Error.Add(iterator.Index, "AnyCharacter: ");
                        }
                        return result;
                    }
                )
             );
        }

        public override void Visit(CharacterClass expression)
        {
            String characterclass = expression.ClassExpression;
            this.matchStack.Push(
                new IsMatchPredicate(
                    delegate(InputIterator iterator)
                    {
                        Boolean result = Regex.IsMatch(iterator.Current.ToString(), characterclass);
                        if (result)
                        {
                            iterator.Next();
                        }
                        else
                        {
                            if(!this.Error.ContainsKey(iterator.Index))
                                this.Error.Add(iterator.Index, "CharacterClass: " + characterclass);
                        }
                        return result;
                    }
                )
             );
        }

        public override void Visit(RecursionCall expression)
        {
            String delegatePointer = expression.FunctionName;
            this.matchStack.Push(
                new IsMatchPredicate(
                    delegate(InputIterator iterator)
                    {
                        return this.recursiveMethod[delegatePointer](iterator);
                    }
                )
             );
        }


        #warning not sure if dynamic reference should be a stack, maybe we shouldn't be popping either as it might need to be referenced several times?
        public override void Visit(DynamicBackReference expression)
        {
            String backreferencename = expression.BackReferenceName;
            Boolean iscasesensitive = expression.IsCaseSensitive;

            this.matchStack.Push(
                new IsMatchPredicate(
                    delegate(InputIterator iterator)
                    {
                        String matchText = String.Empty;
                        if (disableBackReferencePop.Count <= 0)
                            matchText = this.backReferenceLookup[backreferencename].Pop();
                        else
                            matchText = this.backReferenceLookup[backreferencename].Peek();

                        foreach (Char c in matchText)
                        {
                            if (iscasesensitive)
                            {
                                if (iterator.Current != c)
                                {
                                    iterator.Next();
                                    if (!this.Error.ContainsKey(iterator.Index))
                                        this.Error.Add(iterator.Index, "DynamicBackReference: " + matchText);
                                    return false;
                                }
                            }
                            else
                            {
                                if (iterator.Current.ToString().ToUpper() != c.ToString().ToUpper())
                                {
                                    iterator.Next();

                                    if (!this.Error.ContainsKey(iterator.Index))
                                        this.Error.Add(iterator.Index, "DynamicBackReference: " + matchText);
                                    return false;
                                }
                            }
                            iterator.Next();
                        }

                        return true;
                    }
                )
             );
        }
        #endregion

    }
}
