module Ast exposing (..)

import Elm.Syntax.Comments exposing (Comment)
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Documentation exposing (Documentation)
import Elm.Syntax.Exposing exposing (Exposing(..), TopLevelExpose)
import Elm.Syntax.Expression exposing (Case, CaseBlock, Cases, Expression(..), Function, FunctionImplementation, Lambda, LetBlock, LetDeclaration(..), RecordSetter)
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Infix exposing (InfixDirection)
import Elm.Syntax.Module exposing (DefaultModuleData, EffectModuleData, Module(..))
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern, QualifiedNameRef)
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.Signature exposing (Signature)
import Elm.Syntax.Type exposing (Type, ValueConstructor)
import Elm.Syntax.TypeAlias exposing (TypeAlias)
import Elm.Syntax.TypeAnnotation exposing (RecordDefinition, RecordField, TypeAnnotation(..))


type Focused t context
    = Focused Range t context


type Context for_ parentContext previousElements nextElements
    = Context Range parentContext previousElements nextElements


type alias ListContext a parentContext =
    Context (List (Node a)) parentContext (List (Node a)) (List (Node a))



--


type FocusedNode
    = FocusedFile FocusedFile
    | FocusedModuleDefinition FocusedModuleDefinition
    | FocusedImport FocusedImport
    | FocusedComment FocusedComment
    | FocusedExposing FocusedExposing
    | FocusedTopLevelExpose FocusedTopLevelExpose
    | FocusedDeclaration FocusedDeclaration
    | FocusedDocumentation FocusedDocumentation
    | FocusedCustomType FocusedCustomType
    | FocusedTypeAlias FocusedTypeAlias
    | FocusedTypeAnnotation FocusedTypeAnnotation
    | FocusedSignature FocusedSignature
    | FocusedValueConstructor FocusedValueConstructor
    | FocusedFunction FocusedFunction
    | FocusedFunctionImplementation FocusedFunctionImplementation
    | FocusedRecordDefinition FocusedRecordDefinition
    | FocusedRecordField FocusedRecordField
    | FocusedLetDeclaration FocusedLetDeclaration
    | FocusedLetBlock FocusedLetBlock
    | FocusedExpression FocusedExpression
    | FocusedRecordSetter FocusedRecordSetter
    | FocusedLambda FocusedLambda
    | FocusedCaseBlock FocusedCaseBlock
    | FocusedCase FocusedCase
    | FocusedPattern FocusedPattern
    | FocusedModuleName FocusedModuleName
    | FocusedFunctionName FocusedFunctionName
    | FocusedRecordFieldName FocusedRecordFieldName
    | FocusedGenericType FocusedGenericType
    | FocusedTypeName FocusedTypeName



--


type FileParent
    = FileInProject (Context File () (List File) (List File))


type alias FocusedFile =
    Focused File FileParent


type ModuleDefinitionParent
    = ModuleDefinitionInFile
        (Context File
            FileParent
            ()
            { imports : List (Node Import)
            , declarations : List (Node Declaration)
            , comments : List (Node Comment)
            }
        )


type alias FocusedModuleDefinition =
    Focused Module ModuleDefinitionParent


type ImportParent
    = ImportsInFile
        (ListContext Import
            (Context File
                FileParent
                { moduleDefinition : Node Module }
                { declarations : List (Node Declaration)
                , comments : List (Node Comment)
                }
            )
        )


type alias FocusedImport =
    Focused Import ImportParent


type CommentParent
    = CommentsInFile
        (ListContext Comment
            (Context File
                FileParent
                { moduleDefinition : Node Module
                , imports : List (Node Import)
                , declarations : List (Node Declaration)
                }
                ()
            )
        )


type alias FocusedComment =
    Focused Comment (ListContext Comment CommentParent)


type ExposingParent
    = ExposingInImport
        (Context Import
            ImportParent
            { moduleName : Node ModuleName
            , moduleAlias : Maybe (Node ModuleName)
            }
            ()
        )
    | ExposingInNormalModule
        (Context DefaultModuleData
            ModuleDefinitionParent
            { moduleName : Node ModuleName
            }
            ()
        )
    | ExposingInPortModule
        (Context DefaultModuleData
            ModuleDefinitionParent
            { moduleName : Node ModuleName
            }
            ()
        )
    | ExposingInEffectModuleData
        (Context EffectModuleData
            ModuleDefinitionParent
            { moduleName : Node ModuleName }
            { command : Maybe (Node String)
            , subscription : Maybe (Node String)
            }
        )


type alias FocusedExposing =
    Focused Exposing ExposingParent


type TopLevelExposeParent
    = TopLevelExposeInExplicit (ListContext TopLevelExpose ExposingParent)


type alias FocusedTopLevelExpose =
    Focused TopLevelExpose TopLevelExposeParent


type DeclarationParent
    = DeclarationInFile
        (ListContext Declaration
            (Context File
                FileParent
                { moduleDefinition : Node Module
                , imports : List (Node Import)
                }
                { comments : List (Node Comment)
                }
            )
        )


type alias FocusedDeclaration =
    Focused Declaration DeclarationParent


type DocumentationParent
    = DocumentationInFunction
        (Context Function
            FunctionParent
            ()
            { signature : Maybe (Node Signature)
            , declaration : Node FunctionImplementation
            }
        )
    | DocumentationInCustomType
        (Context Type
            CustomTypeParent
            ()
            { name : Node String
            , generics : List (Node String)
            , constructors : List (Node ValueConstructor)
            }
        )
    | DocumentationInTypeAlias
        (Context TypeAlias
            TypeAliasParent
            ()
            { name : Node String
            , generics : List (Node String)
            , typeAnnotation : Node TypeAnnotation
            }
        )


type alias FocusedDocumentation =
    Focused Documentation DocumentationParent


type alias CustomTypeParent =
    DeclarationParent


type alias FocusedCustomType =
    Focused Type CustomTypeParent


type alias TypeAliasParent =
    DeclarationParent


type alias FocusedTypeAlias =
    Focused TypeAlias TypeAliasParent


type TypeAnnotationParent
    = TypeAnnotationInSignature
        (Context Signature
            SignatureParent
            { name : Node String
            }
            ()
        )
    | TypeAnnotationInValueConstructor
        (ListContext TypeAnnotation
            (Context ValueConstructor
                ValueConstructorParent
                { name : Node String
                }
                ()
            )
        )
    | TypeAnnotationInTypeAlias
        (Context TypeAlias
            TypeAliasParent
            { documentation : Maybe (Node Documentation)
            , name : Node String
            , generics : List (Node String)
            }
            ()
        )
    | TypeAnnotationInTyped (ListContext TypeAnnotation (Context TypeAnnotation TypeAnnotationParent (Node ( ModuleName, String )) ()))
    | TypeAnnotationInTupled (ListContext TypeAnnotation TypeAnnotationParent)
    | TypeAnnotationInFunctionTypeAnnotationFrom (Context TypeAnnotation TypeAnnotationParent () (Node TypeAnnotation))
    | TypeAnnotationInFunctionTypeAnnotationTo (Context TypeAnnotation TypeAnnotationParent (Node TypeAnnotation) ())
    | TypeAnnotationInRecordField (Context RecordField RecordFieldParent (Node String) ())


type alias FocusedTypeAnnotation =
    Focused TypeAnnotation TypeAnnotationParent


type SignatureParent
    = SignatureInPortDeclaration DeclarationParent
    | SignatureInFunction
        (Context Function
            FunctionParent
            { documentation : Maybe (Node Documentation) }
            { declaration : Node FunctionImplementation
            }
        )


type alias FocusedSignature =
    Focused Signature SignatureParent


type ValueConstructorParent
    = ValueConstructorInType
        (ListContext ValueConstructor
            (Context Type
                CustomTypeParent
                { documentation : Maybe (Node Documentation)
                , name : Node String
                , generics : List (Node String)
                }
                ()
            )
        )


type alias FocusedValueConstructor =
    Focused ValueConstructor ValueConstructorParent


type FunctionParent
    = FunctionInDeclaration DeclarationParent
    | FunctionInLetDeclaration LetDeclarationParent


type alias FocusedFunction =
    Focused Function FunctionParent


type FunctionImplementationParent
    = FunctionImplementationInFunction
        (Context Function
            FunctionParent
            { documentation : Maybe (Node Documentation)
            , signature : Maybe (Node Signature)
            }
            ()
        )


type alias FocusedFunctionImplementation =
    Focused FunctionImplementation FunctionImplementationParent


type RecordDefinitionParent
    = RecordDefinitionInRecord (Context TypeAnnotation TypeAnnotationParent () ())
    | RecordDefinitionInGenericRecord (Context TypeAnnotation TypeAnnotationParent (Node String) ())


type alias FocusedRecordDefinition =
    Focused RecordDefinition RecordDefinitionParent


type RecordFieldParent
    = RecordFieldInRecordDefinition (ListContext RecordField RecordDefinitionParent)


type alias FocusedRecordField =
    Focused RecordField (ListContext RecordField RecordDefinitionParent)


type LetDeclarationParent
    = LetDeclarationInLetBlock
        (ListContext LetDeclaration
            (Context LetBlock
                LetBlockParent
                ()
                { expression : Node Expression
                }
            )
        )


type alias FocusedLetDeclaration =
    Focused LetDeclaration LetDeclarationParent


type alias LetBlockParent =
    ExpressionParent


type alias FocusedLetBlock =
    Focused LetBlock LetBlockParent


type ExpressionParent
    = ExpressionInDestructuring (Context Declaration DeclarationParent (Node Pattern) ())
    | ExpressionInFunctionImplementation
        (Context FunctionImplementation
            FunctionImplementationParent
            { name : Node String
            , arguments : List (Node Pattern)
            }
            ()
        )
    | ExpressionInApplication (ListContext Expression ExpressionParent)
    | ExpressionInOperatorApplicationLeft (Context Expression ExpressionParent ( String, InfixDirection ) { right : Node Expression })
    | ExpressionInOperatorApplicationRight (Context Expression ExpressionParent ( String, InfixDirection, { left : Node Expression } ) ())
    | ExpressionInIfBlockIf
        (Context Expression
            ExpressionParent
            ()
            { then_ : Node Expression
            , else_ : Node Expression
            }
        )
    | ExpressionInIfBlockThen (Context Expression ExpressionParent { if_ : Node Expression } { else_ : Node Expression })
    | ExpressionInIfBlockElse
        (Context Expression
            ExpressionParent
            { if_ : Node Expression
            , then_ : Node Expression
            }
            ()
        )
    | ExpressionInNegation (Context Expression ExpressionParent () ())
    | ExpressionInTupledExpression (ListContext Expression ExpressionParent)
    | ExpressionInParenthesizedExpression (Context Expression ExpressionParent () ())
    | ExpressionInListExpr (ListContext Expression ExpressionParent)
    | ExpressionInRecordAccess (Context Expression ExpressionParent () (Node String))
    | ExpressionInRecordSetter (Context Expression ExpressionParent (Node String) ())
    | ExpressionInLetBlock
        (Context LetBlock
            LetBlockParent
            { declarations : List (Node LetDeclaration)
            }
            ()
        )
    | ExpressionInLetDestructuring (Context LetDeclaration LetDeclarationParent (Node Pattern) ())
    | ExpressionInLambda
        (Context Lambda
            LambdaParent
            { args : List (Node Pattern)
            }
            ()
        )
    | ExpressionInCaseBlock (Context CaseBlock CaseBlockParent () { cases : Cases })
    | ExpressionInCase (Context Case CaseParent (Node Pattern) ())


type alias FocusedExpression =
    Focused Expression ExpressionParent


type RecordSetterParent
    = RecordSetterInRecordExpr (ListContext RecordSetter ExpressionParent)
    | RecordSetterInRecordUpdateExpression (ListContext RecordSetter (Context Expression ExpressionParent (Node String) ()))


type alias FocusedRecordSetter =
    Focused RecordSetter RecordSetterParent


type alias LambdaParent =
    ExpressionParent


type alias FocusedLambda =
    Focused Lambda LambdaParent


type alias CaseBlockParent =
    ExpressionParent


type alias FocusedCaseBlock =
    Focused CaseBlock CaseBlockParent


type CaseParent
    = CaseInCaseBlock
        (ListContext Case
            (Context CaseBlock
                CaseBlockParent
                { expression : Node Expression
                }
                ()
            )
        )


type alias FocusedCase =
    Focused Case CaseParent


type PatternParent
    = PatternInDestructuring (Context Declaration DeclarationParent () (Node Expression))
    | PatternInFunctionImplementation
        (ListContext Pattern
            (Context FunctionImplementation
                FunctionImplementationParent
                { name : Node String
                }
                { expression : Node Expression
                }
            )
        )
    | PatternInLetDestructuring (Context LetDeclaration LetDeclarationParent () (Node Expression))
    | PatternInLambda
        (ListContext Pattern
            (Context Lambda
                LambdaParent
                { args : Node Pattern
                }
                { expression : Node Expression
                }
            )
        )
    | PatternInCase (Context Case CaseParent () (Node Expression))
    | PatternInTuplePattern (ListContext Pattern PatternParent)
    | PatternInUnConsPatternFirst (Context Pattern PatternParent () (Node Pattern))
    | PatternInUnConsPatternRest (Context Pattern PatternParent (Node Pattern) ())
    | PatternInListPattern (ListContext Pattern PatternParent)
    | PatternInNamedPattern (ListContext Pattern (Context Pattern PatternParent QualifiedNameRef ()))
    | PatternInAsPattern (Context Pattern PatternParent () (Node String))
    | PatternInParenthesizedPattern (Context Pattern PatternParent () ())


type alias FocusedPattern =
    Focused Pattern PatternParent


type ModuleNameParent
    = ModuleNameInFunctionOrValue (Context Expression ExpressionParent () String) -- this one isn't a node.
    | ModuleNameInImportName
        (Context Import
            ImportParent
            ()
            { moduleAlias : Maybe (Node ModuleName)
            , exposingList : Maybe (Node Exposing)
            }
        )
    | ModuleNameInImportAlias
        (Context Import
            ImportParent
            { moduleName : Node ModuleName
            }
            { exposingList : Maybe (Node Exposing)
            }
        )
    | ModuleNameInNormalModule
        (Context DefaultModuleData
            ModuleDefinitionParent
            ()
            { exposingList : Node Exposing
            }
        )
    | ModuleNameInPortModule
        (Context DefaultModuleData
            ModuleDefinitionParent
            ()
            { exposingList : Node Exposing
            }
        )
    | ModuleNameInEffectModuleData
        (Context EffectModuleData
            ModuleDefinitionParent
            ()
            { exposingList : Node Exposing
            , command : Maybe (Node String)
            , subscription : Maybe (Node String)
            }
        )
    | ModuleNameInTyped (Context TypeAnnotation TypeAnnotationParent () ( String, List (Node TypeAnnotation) )) -- This one is weird because there is also a String in the same node.


type alias FocusedModuleName =
    Focused ModuleName ModuleNameParent


type FunctionNameParent
    = FunctionNameInFunctionImplementation
        (Context FunctionImplementation
            FunctionImplementationParent
            ()
            { arguments : List (Node Pattern)
            , expression : Node Expression
            }
        )
    | FunctionNameInSignature
        (Context Signature
            SignatureParent
            ()
            { typeAnnotation : Node TypeAnnotation
            }
        )


type alias FocusedFunctionName =
    -- todo maybe make this variable name? It doesn't include FunctionOrValue so isn't complete anyway.
    Focused String FunctionNameParent


type RecordFieldNameParent
    = RecordFieldNameInRecordAccess (Context Expression ExpressionParent (Node Expression) ())
    | RecordFieldNameInRecordSetter (Context RecordSetter RecordSetterParent () (Node Expression))
    | RecordFieldNameInRecordPattern (ListContext String (Context Pattern PatternParent () ()))
    | RecordFieldNameInRecordField (Context RecordField RecordFieldParent () (Node TypeAnnotation))


type alias FocusedRecordFieldName =
    Focused String RecordFieldNameParent


type GenericTypeParent
    = GenericTypeInCustomType
        (ListContext String
            (Context Type
                CustomTypeParent
                { documentation : Maybe (Node Documentation)
                , name : Node String
                }
                { constructors : List (Node ValueConstructor)
                }
            )
        )
    | GenericTypeInTypeAlias
        (ListContext String
            (Context TypeAlias
                TypeAliasParent
                { documentation : Maybe (Node Documentation)
                , name : Node String
                }
                { typeAnnotation : Node TypeAnnotation
                }
            )
        )
    | GenericTypeInGenericRecord (Context TypeAnnotation TypeAnnotationParent () (Node RecordDefinition))
    | GenericTypeInTypeAnnotation (Context TypeAnnotation TypeAnnotationParent () ())


type alias FocusedGenericType =
    Focused String GenericTypeParent


type TypeNameParent
    = TypeNameInCustomType
        (Context Type
            CustomTypeParent
            { documentation : Maybe (Node Documentation) }
            { generics : List (Node String)
            , constructors : List (Node ValueConstructor)
            }
        )
    | TypeNameInTypeAlias
        (Context TypeAlias
            TypeAliasParent
            { documentation : Maybe (Node Documentation) }
            { generics : List (Node String)
            , typeAnnotation : Node TypeAnnotation
            }
        )


type alias FocusedTypeName =
    Focused String TypeNameParent
