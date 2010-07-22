import Maybe (fromJust, isNothing)

import Text.ParserCombinators.Parsec    as P
import qualified Text.ParserCombinators.Parsec.Token as Tok
import qualified Text.ParserCombinators.Parsec.Language as PLang
import Text.ParserCombinators.Parsec.Expr

import qualified Text.PrettyPrint as PP
import Text.PrettyPrint ((<>), (<+>), ($$))

import qualified Debug.Trace as Tr

import SashoLib


data FuncDef = FuncDef { qual      :: [Type], -- a qualified type
                         name      :: String,
                         argtypes  :: [Type] }
               deriving (Show)
             

data Type = PlainType String              
            | Qualified [Type] Type     -- a type inside other types, or namespaces
                                        -- (syntactically indistinguishable from plain
                                        -- types)
            | ConstType Type            -- const
            | TemplType String [Either Type Literal]   -- template type
            | PtrType Type                  
            | RefType Type              -- ^ reference type
            | ArrayType Type                
            | Modified Modifier Type -- const etc
            | FuncType          -- a function
                     Type               -- return type
                     (Maybe String)     -- function name
                     [Type]             -- param types
            | Void
                     
            | Unknown            
                  deriving (Show)

data Modifier = Const | Unsigned | Explicit
              deriving (Show)


type NamedType = (String, Type)


data Literal = LitInt Int |
               LitUnsigned Int
    deriving (Show)
             


main = do p <- getContents >>== parse msgParser "stdin"
          case p of (Left err)      -> print err
                    (Right pretty)  -> putStrLn . myrender $ pretty



-- parses one of several options, pretty-prints, and returns a PP.Doc
msgParser = choice $ map try $ [(do p <- expandedTemplateParser
                                    eof
                                    return $ PP.text "expanded template:" $$ docExpandedTemplate p),
                                (do p <- Tr.trace "trying funcDefParser" $ funcDefParser
                                    eof
                                    return $ PP.text "function def:" $$ docFuncDef p),
                                (do p <- Tr.trace "trying typeParser" $ typeParser
                                    eof
                                    return $ PP.text "type:" $$ docType p)]
{-
msgParser = choice $ map try $ map tryKind [(candidateParser, docCandidate),
                                            (funcDefParser, docFuncDef),
                                            (typeParser, docType)]
-}

{-
tryKind (parser, printer) = do p <- parser
                               eof
                               return $ printer p
-}

data ExpandedTemplate = ExpTempl
                                (Maybe Type)-- return type
                                FuncDef     -- function definition
                                            -- template parameter values - either the name
                                            -- of a class, and its instantiated type, or a
                                            -- named type and its instantiated value
                                --                    [Either (String,Type) (NamedType, Literal)] 
                                [(String,Type)]


expandedTemplateParser = do (rettype,def)   <- funcDefParser `withOptionalPrefix` typeParser
                            paramvals       <- brackets (do symbol "with"
                                                            paramValParser `sepBy` comma)
                            return $ ExpTempl rettype def paramvals

paramValParser = do name <- identifier
                    symbol "="
                    val <- typeParser
                    return (name, val)

funcDefParser = do quals    <- innerTypeParser `endByWithBacktrack` reservedOp "::"
                   name     <- (do reserved "operator"
                                   op <- anyReservedOp
                                   return $ "operator" ++ op)
                               <|>
                               identifier
                   argtypes <- parens (typeParser `sepBy` comma)
                                        <?> "function def: argument types"
                   return $ FuncDef quals name argtypes

-- in case 'p' may eat some tokens, and then turn out that sep is not present
endByWithBacktrack p sep = many $ try $ do{ x <- p; sep; return x }

--
-- and the actual types
--

-- a "complex type" - with postfix operations and function types included
typeParser              = (try funcParser) <|> postfixTypeParser

funcParser              = do ret     <- postfixTypeParser
                             name    <- parens (do (reservedOp "*" <|> reservedOp "&" <|> return ())
                                                   maybeParse identifier)
                                                 <?> "function type: name"
                             params  <- parens (typeParser `sepBy` comma)
                                                 <?> "function type: argument types"
                             return $ FuncType ret name params

postfixTypeParser        = do t <- (try qualifiedParser) <|> innerTypeParser
                              checkOps t [ ("*",     PtrType),
                                           ("[]",    ArrayType),
                                           ("&",     RefType) ]
    where checkOps t symMap = let (syms, funs) = unzip symMap
                                 -- get a list of the symbols present here
                              in  do thesyms <- many $ choice $ map symbol syms
                                         -- the corresponding Type constructors
                                     let constrs = map (symFun symMap) thesyms
                                     -- iterate application of the constructors,
                                     -- starting with t
                                     return $ iterateList t constrs
          symFun map sym    = fromJust $ lookup sym map

-- parse a qualified type
                                   -- make sure we get at least one "::"
qualifiedParser          =      do first <- innerTypeParser `followedBy` reservedOp "::"
                                   rest  <- innerTypeParser `sepBy1` reservedOp "::"
                                   let ts       = (first:rest)
                                       quals    = init ts
                                       t        = last ts
                                   return $ Qualified quals t

-- a type without any immediate postfix operators, those are picked up by postfixTypeParser
innerTypeParser          = choice $ map try [unknownParser        <?> "unknown type",
                                        voidParser           <?> "void",
                                        modifiedTypeParser   <?> "modified type",
                                        templParser          <?> "template",
                                        plainTypeParser      <?> "plain type"]

modifiedTypeParser       = tryQualifiers [("const", Const),
                                          ("unsigned", Unsigned),
                                          ("typename", Explicit)]
    where tryQualifiers qualifiers = choice $ map tryQual qualifiers
          tryQual (name, mod)      = do reserved name
                                        t <- typeParser
                                        return $ Modified mod t

plainTypeParser          = identifier >>== PlainType

unknownParser            = do symbol "<unknown type>"
                              return Unknown

templParser              = do tempName <- identifier
                              ts <- angles $
                                    (templTypeParser <|> templLitParser) `sepBy1` comma
                              return $            TemplType tempName ts
    where templTypeParser = typeParser >>== Left
          templLitParser  = literalParser >>== Right

voidParser               = do reserved "void"
                              return Void
literalParser            = do i <- decimal >>== fromInteger
                              u <- maybeParse $ symbol "u"
                              return $ if isNothing u
                                       then LitInt i
                                       else LitUnsigned i



parseQualifiedTypes = innerTypeParser `sepBy` reservedOp "::"

-- | if fs = [f1,f2,f3...fn], return (fn . ... . f2 . f1) x
iterateList x fs = foldl (flip ($)) x fs


-- note, this swallows but does not anyhow return 'sep'
followedBy p sep = do x <- p
                      sep
                      return x

anyReservedOp :: Parser String
anyReservedOp = choice $ map (\op -> reservedOp op >> return op) $
                             PLang.reservedOpNames cxxLangDef



--
-- set up the lexer
--
cxxLangDef = PLang.javaStyle
             { PLang.reservedOpNames       = [ "::", "*", "&", "()", "+", "==", "!=" ],
               PLang.reservedNames         = [ "const", "unsigned", "typename", "void", "operator" ],
               PLang.identStart            = letter <|>  char '_'
             }

lexer :: Tok.TokenParser ()
lexer  = Tok.makeTokenParser cxxLangDef




--
-- and pretty printing
--

docFuncDef (FuncDef quals name argtypes) = PP.hang ((PP.hcat $ map ((<> PP.text "::") . docType) quals) <>
                                                    (PP.text name))
                                              4
                                              (PP.parens (vertCommas docType argtypes))

docExpandedTemplate (ExpTempl rettype def paramvals) =
    maybe PP.empty docType rettype      $$
    docFuncDef def                      $$
    PP.brackets (vertCommas docParamVal paramvals)
    where docParamVal (name,val) = PP.text name <+> PP.equals <+> docType val


docType t = case t of
              (PlainType name)  -> PP.text name
              (Qualified quals
                         t')    -> PP.hcat $
                                   PP.punctuate (PP.text "::")
                                                (map docType (quals ++ [t']))
              (Modified mod t') -> (docMod mod) <+> docType t'
              (PtrType t')      -> docType t' <> PP.text "*"
              (RefType t')      -> docType t' <> PP.text "&"
              (ArrayType t')    -> docType t' <> PP.text "[]"
              (Unknown)         -> PP.text "<unknown type>"
              (TemplType nm ps) -> PP.text nm <>
                                   docAngles (vertCommas docTemplParam ps)
              (Void)            -> PP.text "void"
              (FuncType ret nm ts)
                                -> docType ret <>
                                   PP.parens (PP.text "*" <> maybe PP.empty PP.text nm) <>
                                   PP.parens (vertCommas docType ts)
    where docTemplParam (Left t)  = docType t
          docTemplParam (Right l) = docLiteral l

docLiteral (LitInt i)       = PP.int i
docLiteral (LitUnsigned u)  = PP.int u <> PP.text "u"

docMod mod = PP.text $ case mod of
                         Unsigned -> "unsigned"
                         Const    -> "const"
                         Explicit -> "typename"




-- print a list with elements stacked vertically and commas after each one, using the
-- element printer 'doc'
vertCommas doc = (PP.vcat .
                  PP.punctuate PP.comma .
                  map doc)


docAngles d = PP.char '<' <> d <> PP.char '>'


myrender = let style = PP.Style { PP.mode = PP.PageMode,
                                  PP.lineLength = 200,
                                  PP.ribbonsPerLine = 1.5 }
           in  PP.renderStyle style



-- routine boring lexer stuff
symbol          = Tok.symbol lexer
lexeme          = Tok.lexeme lexer
parens      = Tok.parens lexer
braces      = Tok.braces lexer
identifier  = Tok.identifier lexer
reserved    = Tok.reserved lexer
operator        = Tok.operator lexer
reservedOp      = Tok.reservedOp lexer
angles          = Tok.angles lexer
comma           = Tok.comma lexer
brackets        = Tok.brackets lexer
decimal         = Tok.decimal lexer


passed_tests = [
         ("operator()", "typename boost::_bi::result_traits<R, F>::type boost::_bi::bind_t<R, F, L>::operator()() [with R = CountedByteArray, F = boost::_mfi::mf1<CountedByteArray, HostIO, const ByteBuffer&>, L = boost::_bi::list2<boost::_bi::value<HostIO*>, boost::arg<2> >]"),

         ("qualified function def", "FlatIO::read(pir::transform_range<std::vector<unsigned int, std::allocator<unsigned int> >, std::pointer_to_unary_function<unsigned int, object_id> >, __gnu_cxx::__normal_iterator<CountedByteArray*, std::vector<CountedByteArray, std::allocator<CountedByteArray> > >)"),

         ("constructor def", "std::pair<_T1, _T2>::pair(const std::pair<_U1, _U2>&) [with _U1 = unsigned int, _U2 = unsigned int, _T1 = boost::array<unsigned int, 2u>, _T2 = boost::array<unsigned int, 2u>]"),

         ("very qualified function def", "stream_processor<Array::dummy_fetches_stream_prog, pir::transform_range<boost::iterator_range<boost::counting_iterator<unsigned int, boost::use_default, boost::use_default> >, scalar2pair<unsigned int> >, 1u, 2u>::process(const Array::dummy_fetches_stream_prog&, const pir::transform_range<boost::iterator_range<boost::counting_iterator<unsigned int, boost::use_default, boost::use_default> >, scalar2pair<unsigned int> >&, boost::array<FlatIO*, 2u>&, boost::array<FlatIO*, 2u>&)"),

         ("function type", "void (*kuku)(u_int, const ByteBuffer&, ByteBuffer&)"),

         ("long func def including a function type", "stream_processor<void ()(u_int, const ByteBuffer&, ByteBuffer&), boost::iterator_range<boost::transform_iterator<std::pointer_to_unary_function<const unsigned int&, std::pair<unsigned int, unsigned int> >, boost::counting_iterator<unsigned int, boost::use_default, boost::use_default>, boost::use_default, boost::use_default> >, 1u, unsigned int, CountedByteArray, std::unary_function<CountedByteArray, ByteBuffer&> >::process(void (&)(u_int, const ByteBuffer&, ByteBuffer&), const boost::iterator_range<boost::transform_iterator<std::pointer_to_unary_function<const unsigned int&, std::pair<unsigned int, unsigned int> >, boost::counting_iterator<unsigned int, boost::use_default, boost::use_default>, boost::use_default, boost::use_default> >&, FlatIO*&, FlatIO*&, addressof<CountedByteArray>)")
          ]


failed_tests = [
                ("const member function", "typename boost::_bi::result_traits<R, F>::type boost::_bi::bind_t<R, F, L>::kuku () const [with R = CountedByteArray, F = boost::_mfi::mf1<CountedByteArray, HostIO, const ByteBuffer&>, L = boost::_bi::list2<boost::_bi::value<HostIO*>, boost::arg<2> >]"),

                ("template instantiation with literal values for named types", "void stream_processor<ItemProc, StreamOrder, B, I>::process(ItemProc&, const StreamOrder&, typename make_maybe_array<FlatIO*, I>::type&, typename make_maybe_array<FlatIO*, I>::type&) [with ItemProc = generate_proc_adapter<fconst<CountedByteArray> >, StreamOrder = pir::transform_range<boost::iterator_range<boost::counting_iterator<unsigned int, boost::use_default, boost::use_default> >, scalar2pair<unsigned int> >, unsigned int B = 1u, unsigned int I = 1u]")
                ]


{-
from gdb:
#8  0x0805d0bb in host_write_blobs<pir::transform_range<pir::transform_range<pir::slice_range<boost::transform_iterator<get_second<boost::array<unsigned int, 2u>, boost::array<unsigned int, 2u> >, boost::transform_iterator<scalar2pair<boost::array<unsigned int, 2u> >, boost::transform_iterator<scalar2array<unsigned int, 2u>, boost::counting_iterator<unsigned int, boost::use_default, boost::use_default>, boost::use_default, boost::use_default>, boost::use_default, boost::use_default>, boost::use_default, boost::use_default> >, std::pointer_to_unary_function<unsigned int, object_id> >, std::pointer_to_unary_function<object_id const&, object_name_t> >, pir::transform_range<pir::slice_range<__gnu_cxx::__normal_iterator<boost::array<CountedByteArray, 2u> const*, std::vector<boost::array<CountedByteArray, 2u>, std::allocator<boost::array<CountedByteArray, 2u> > > > >, HostIO::do_outfilter> >

weird, first thing is in parens. what does this mean??
/usr/lib/gcc/i486-linux-gnu/4.0.3/../../../../include/c++/4.0.3/bits/stl_algo.h:824: error: no match for call to '(boost::_bi::bind_t<CountedByteArray, boost::_mfi::mf1<CountedByteArray, HostIO, const ByteBuffer&>, boost::_bi::list2<boost::_bi::value<HostIO*>, boost::arg<2> > >) (object_id, CountedByteArray&)'


to do:


stream/processor.h:62: note: candidates are: static void stream_processor<ItemProc, StreamOrder, N, IdxBatch, ObjBatch, MkOutObjsT>::process(ItemProc&, const StreamOrder&, FlatIO*, FlatIO*, MkOutObjsT) [with ItemProc = void ()(u_int, const ByteBuffer&, ByteBuffer&), StreamOrder = boost::iterator_range<boost::transform_iterator<std::pointer_to_unary_function<const unsigned int&, std::pair<unsigned int, unsigned int> >, boost::counting_iterator<unsigned int, boost::use_default, boost::use_default>, boost::use_default, boost::use_default> >, unsigned int N = 1u, IdxBatch = unsigned int, ObjBatch = CountedByteArray, MkOutObjsT = std::unary_function<CountedByteArray, ByteBuffer&>]


-}
