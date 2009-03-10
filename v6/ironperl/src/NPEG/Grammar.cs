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

using NPEG.Terminals;
using NPEG.NonTerminals;

namespace NPEG
{
    public static class Grammar
    {
        public static AExpression Load(String rules)
        {
            String TERMINALNAME = "[a-zA-Z0-9_]";

            AExpression Digits = new CapturingGroup("Digit", new CharacterClass(){ ClassExpression="[0-9]"});
            AExpression S = new ZeroOrMore(new CharacterClass() { ClassExpression = "[ \n\r\t\v]" });



            #region capturing group
            // ReplacementNode: ('\rn=['[a-zA-Z.]+ ',' S [a-zA-Z.]+ ']');
            AExpression CG_ReplacementNode = new CapturingGroup("ReplacementNode",
                                                            new Sequence(
                                                                new Literal() { MatchText = "\rn=[" }
                                                                ,
                                                                new CapturingGroup("ClassName", 
                                                                    new OneOrMore(
                                                                        new CharacterClass() { ClassExpression = "[a-zA-Z.]" }
                                                                    )
                                                                )
                                                            )
                                                            .Sequence(
                                                                new Literal(){ MatchText=","}
                                                            )
                                                            .Sequence(S)
                                                            .Sequence(
                                                                new CapturingGroup("AssemblyName", 
                                                                    new OneOrMore(
                                                                        new CharacterClass() { ClassExpression = "[a-zA-Z.]" }
                                                                    )
                                                                )
                                                            )
                                                            .Sequence(
                                                                new Literal() { MatchText = "]" }
                                                            )
                                                         );

            // ReplaceBySingleChild: '\rsc';
            AExpression CG_ReplaceBySingleChild = new CapturingGroup("ReplaceBySingleChild", new Literal() { MatchText = "\rsc" });

            // Options: ( ReplaceBySingleChild ReplacementNode / ReplacementNode ReplaceBySingleChild / ReplaceBySingleChild / ReplacementNode )?
            AExpression CG_Options = new Optional(
                                        new PrioritizedChoice(
                                            new Sequence(
                                                CG_ReplaceBySingleChild
                                                ,
                                                CG_ReplacementNode
                                            )
                                            ,
                                            new Sequence(
                                                CG_ReplacementNode
                                                ,
                                                CG_ReplaceBySingleChild
                                            )
                                         )
                                         .Or(
                                            CG_ReplacementNode
                                         )
                                         .Or(
                                            CG_ReplaceBySingleChild
                                         )
                                     );

            // CapturingGroup: '(' '?' '<' [a-zA-Z]+ Options '>' ')' / '(' '?' '<' [a-zA-Z]+ Options '>' RecursiveExpression? ')'
            AExpression CapturingGroup = new CapturingGroup("CapturingGroup",
                                             new Sequence(
                                                new Literal() { MatchText = "(" },
                                                new Literal() { MatchText = "?" }
                                             )
                                             .Sequence(
                                                new Literal() { MatchText = "<" }
                                             )
                                             .Sequence(
                                                new CapturingGroup("Name", 
                                                    new OneOrMore(
                                                        new CharacterClass() { ClassExpression = TERMINALNAME }
                                                    )
                                                )
                                             )
                                             .Sequence(
                                                CG_Options
                                             )
                                             .Sequence(
                                                new Literal() { MatchText = ">" }
                                             )
                                             .Sequence(
                                                new Optional(
                                                    new RecursionCall("ExpressionFunction")
                                                )
                                             )
                                             .Sequence(
                                                new Literal() { MatchText = ")" }
                                             )
                                         );
            #endregion




            // Expression: CharacterClass / Literal / AnyCharacter
            //             '[' (!']'.)* ']' / '"' (!'"'.)* '"'  / "."
            AExpression Terminal = new PrioritizedChoice(
                                        new CapturingGroup("CharacterClass",
                                            new Sequence(
                                                new Literal() { MatchText = "[" },
                                                new OneOrMore(new CharacterClass() { ClassExpression = @"[^\]]" })
                                            )
                                            .Sequence(new Literal() { MatchText = "]" })
                                        )
                                        ,
                                        new CapturingGroup("Literal",
                                            new Sequence(
                                                new PrioritizedChoice(
                                                    new Sequence(
                                                        new Literal() { MatchText = "\"" },
                                                        new CapturingGroup("Text",
                                                            new OneOrMore(
                                                                new PrioritizedChoice(
                                                                    new Sequence(
                                                                        new AndPredicate(
                                                                            new Literal(){ MatchText = @"\""" }
                                                                        )
                                                                        ,
                                                                        // consumes to characters \" until it finds " not escaped
                                                                        new LimitingRepetition(new AnyCharacter()){ Min=2, Max=2}
                                                                    )
                                                                    ,
                                                                    new CharacterClass() { ClassExpression = "[^\"]" }
                                                                )
                                                            )
                                                        )
                                                    )
                                                    .Sequence(new Literal() { MatchText = "\"" })
                                                    ,
                                                    new Sequence(
                                                        new Literal() { MatchText = "'" },
                                                        new CapturingGroup("Text",
                                                            new OneOrMore(
                                                                new PrioritizedChoice(
                                                                    new Sequence(
                                                                        new AndPredicate(
                                                                            new Literal(){ MatchText = @"\'" }
                                                                        )
                                                                        ,
                                                                        // consumes to characters \' until it finds ' not escaped
                                                                        new LimitingRepetition(new AnyCharacter()){ Min=2, Max=2}
                                                                    )
                                                                    ,
                                                                    new CharacterClass() { ClassExpression = "[^']" }
                                                                )
                                                            )
                                                        )
                                                    )
                                                    .Sequence(new Literal() { MatchText = "'" })
                                                )
                                                ,
                                                // optional do case-insensitive pattern matching. 
                                                new Optional(new CapturingGroup("CaseInsensitive", new Literal() { MatchText = @"\i" }))
                                            )
                                        )
                                    )
                                    .Or(
                                        new CapturingGroup("AnyCharacter",
                                            new Literal() { MatchText = "." }
                                        )
                                    )
                                    .Or(
                                        new CapturingGroup("DynamicBackReferencing",
                                            new Sequence(
                                                new Literal() { MatchText = @"\k<" }
                                                ,
                                                new CapturingGroup("Name", new OneOrMore(new CharacterClass() { ClassExpression = TERMINALNAME }))
                                            )
                                            .Sequence(
                                                new Optional(
                                                    new Sequence(
                                                        new Sequence(
                                                            new Literal() { MatchText="["}
                                                            ,
                                                            new CapturingGroup("CaseInsensitive",
                                                                new Literal() { MatchText = @"\i" }
                                                            )
                                                        )
                                                        ,
                                                        new Literal() { MatchText="]"}
                                                    )
                                                )
                                            )
                                            .Sequence(
                                                new Literal(){ MatchText=">"}
                                            )
                                        )
                                    )
                                    .Or(
                                        new CapturingGroup("TerminalReference",
                                            new OneOrMore(new CharacterClass() { ClassExpression = TERMINALNAME })
                                        )
                                    )
                                    .Or(
                                        CapturingGroup
                                    )
                                    .Or(
                                        new CapturingGroup("Group",
                                            new Sequence(
                                                new Sequence(
                                                    new Literal() { MatchText = "(" }
                                                    , 
                                                    S
                                                )
                                                ,
                                                new RecursionCall("ExpressionFunction")
                                            )
                                            .Sequence(S)
                                            .Sequence(new Literal() { MatchText = ")" })
                                        )// { DoReplaceBySingleChildNode = true }
                                    );



            AExpression Suffix =    new PrioritizedChoice(
                                        new CapturingGroup("ZeroOrMore", new Literal() { MatchText = "*" })
                                        ,
                                        new CapturingGroup("OneOrMore", new Literal() { MatchText = "+" })
                                    )
                                    .Or(
                                        new CapturingGroup("Optional", new Literal() { MatchText = "?" })
                                    )
                                    .Or(
                                        new CapturingGroup("LimitingRepetition",
                                            new Sequence(
                                                new Literal() { MatchText = "{" }
                                                ,
                                                new PrioritizedChoice(
                                                    // {min,max}
                                                    new CapturingGroup("MIN_MAX",
                                                        new Sequence(
                                                            new OneOrMore(Digits),
                                                            new Literal() { MatchText = "," }
                                                        )
                                                        .Sequence(
                                                            new OneOrMore(Digits)
                                                        )
                                                    )
                                                    ,
                                                    //{,max}
                                                    new CapturingGroup("_MAX",
                                                        new Sequence(
                                                            new Literal() { MatchText = "," }
                                                            ,
                                                            new OneOrMore(Digits)
                                                        )
                                                    )
                                                )
                                                .Or
                                                (
                                                    //{min,}
                                                    new CapturingGroup("MIN_",
                                                        new Sequence(
                                                            new OneOrMore(Digits)
                                                            ,
                                                            new Literal() { MatchText = "," }
                                                        )
                                                    )
                                                )
                                                .Or
                                                (
                                                    new OneOrMore(Digits)
                                                )
                                            )
                                            .Sequence(
                                               new Literal() { MatchText = "}" }
                                            )
                                        )
                                    );




            AExpression Prefix = new PrioritizedChoice(
                                    new Literal() { MatchText = "!" }
                                    ,
                                    new Literal() { MatchText = "&" }
                                );




            Terminal = new PrioritizedChoice(
                            new CapturingGroup("Prefix", new Sequence(Prefix, Terminal))
                            ,
                            new CapturingGroup("Suffix", new Sequence(Terminal, Suffix))
                        ).Or(
                            Terminal
                        );

            Terminal = new Sequence(S, Terminal).Sequence(S);







            AExpression SequenceExpression =  new CapturingGroup("Sequence",
                                                    new OneOrMore(Terminal)
                                              ) 
                                              { 
                                                  DoReplaceBySingleChildNode = true 
                                              };

            AExpression PrioritizedChoiceExpression = new CapturingGroup("PrioritizedChoice",
                                                            new PrioritizedChoice(
                                                                new Sequence(
                                                                    new Sequence(
                                                                        SequenceExpression
                                                                        ,
                                                                        new Literal() { MatchText = "/" }
                                                                    )
                                                                    ,
                                                                    SequenceExpression
                                                                )
                                                                ,
                                                                // when continually matching or statements () / () / () / ()
                                                                new Sequence(
                                                                    new Sequence(
                                                                        S
                                                                        ,
                                                                        new Literal() { MatchText = "/" }
                                                                    )
                                                                    ,
                                                                    SequenceExpression
                                                                )
                                                            )
                                                      );

            AExpression Expression = new RecursionCreate("ExpressionFunction",
                                        new PrioritizedChoice(
                                            PrioritizedChoiceExpression
                                            ,
                                            SequenceExpression
                                        ).Plus()
                                     );


            // OrderedChoice will always contain 1 child.
            // Visitor Interpretor has logic to require one child for ordered choice.

            // DoReplaceBySingleChildNode is set on Sequence, 
            // if only a single child node exists. Sequence is collapsed since it cannot exist without a left and a right.
            // if it has a left and a right we keep the terminal and visitor creates the sequence object.

            Expression = new Sequence(S, Expression).Sequence(S);

            AExpression Statement = new CapturingGroup(
                                        "Statement", 
                                        new GrammarInterpreter.StatementAstNode(),
                                        new Sequence(
                                            new CapturingGroup("NodeDefinition",
                                                new PrioritizedChoice(
                                                    new OneOrMore(
                                                        new CharacterClass() { ClassExpression = TERMINALNAME }
                                                    )
                                                    ,
                                                    CapturingGroup
                                                )
                                            )
                                            ,
                                            new Literal() { MatchText = ":" }
                                        )
                                        .Sequence(new OneOrMore(Expression))
                                        .Sequence(
                                            new Literal() { MatchText = ";" }
                                        )
                                    );

            AExpression ROOT = new CapturingGroup(
                                        "PEG", 
                                        new GrammarInterpreter.Interpreter(),
                                        //new Sequence( 
                                            new Sequence(S, Statement).Plus()
                                        //    , 
                                        //    new NotPredicate(
                                        //        new AnyCharacter()
                                        //    )
                                        //)
                               );



            using (CompositeGrammarCreatorVisitor grammarcreator = new CompositeGrammarCreatorVisitor())
            {
                ROOT.Accept(grammarcreator);
                String o = grammarcreator.GrammarOutput;
            }


            CompositeMatchVisitor visitor = new CompositeMatchVisitor(
                                        new InputIterator(rules)
                                     );
            ROOT.Accept(visitor);
            if (visitor.IsMatch)
            {
                GrammarInterpreter.Interpreter interpret = visitor.AST as GrammarInterpreter.Interpreter;


#if (DIAGNOSTICS)
                using (CompositePrintParseTreeVisitor parsetree = new CompositePrintParseTreeVisitor())
                {
                    ROOT.Accept(parsetree);
                }

                System.Diagnostics.Debug.WriteLine("");
                System.Diagnostics.Debug.WriteLine("");
                System.Diagnostics.Debug.WriteLine("");
                ASTPrintVisitor print = new ASTPrintVisitor();
                interpret.Accept(print);
                print.Dispose();
                System.Diagnostics.Debug.WriteLine("");
                System.Diagnostics.Debug.WriteLine("");
#endif
                return interpret.Expression;
            }
            else
            {
                throw new Exception("Could not create AST");
            }
        }
    }
}
