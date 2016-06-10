import Text.ParserCombinators.Parsec
import Data.Char
import Data.Bool

data Expr = E Expr Expr | OU Expr Expr | NAO Expr | BI Expr Expr | IM Expr Expr | VAR String | C Bool deriving Show

main = do {putStr "\nExpressao:";
  e <- getLine;
  case avaliarExpr e of
    Left err -> putStr ((show err)++ "\n")--aki
    Right r  -> roraTudo r}

avaliarExpr e = parse expr "Erro:" e --aki

ret v1 Nothing = v1
ret v1 (Just(op,v2)) = op v1 v2
  
-- Especificacao sintatica

expr = do
  v1 <- term  -- E -> TE'
  e <- expr'
  return (ret v1 e)

expr' = do {char '&'; -- E' -> &TE'
  v1 <- term;
  e <- expr';
  return (Just ((E), ret v1 e))}
  <|>
  do {char '|'; -- E' -> |TE'
    v1 <- term;
    e <- expr';
    return (Just ((OU), ret v1 e))}
  <|> return Nothing -- E' -> vazio 

term = do
  v1 <- fator -- T -> FT'
  e <- term'
  return (ret v1 e)

term' = do {string "->"; -- T' ->    ->FT' --aki
  v1 <- fator;
  e <- term';
  return (Just ((IM), ret v1 e))} 
  <|>
  do {string "<->"; -- T' ->     <->FT'
    v1 <- fator;
    e <- term';
    return (Just ((BI), ret v1 e))}
  <|> return Nothing -- T' -> vazio

fator = do {s <- id1; return (VAR s) } --F -> id1
  <|> do {char '~'; e <- fator; return (NAO e)} -- F -> NAO F 
  <|> do {char '('; e <- expr; char ')'; return e} -- F -> (E)

-- Especificacao lexica
id1 = do {l <- letter; ls <- many (letter <|> digit); return (l:ls)}--aki


-- faz o calculo
resultado (VAR str) tuplaIdBool = valor_atual str tuplaIdBool
resultado (NAO a) tuplaIdBool = not (resultado a tuplaIdBool)
resultado (E a b) tuplaIdBool = resultado a tuplaIdBool && resultado b tuplaIdBool
resultado (OU a b) tuplaIdBool = resultado a tuplaIdBool || resultado b tuplaIdBool
resultado (IM a b) tuplaIdBool = not (resultado a tuplaIdBool) || resultado b tuplaIdBool
resultado (BI a b) tuplaIdBool = not (resultado a tuplaIdBool) || resultado b tuplaIdBool && resultado a tuplaIdBool || not (resultado b tuplaIdBool)

--retorna uma tupla com o id e o booleano que o d representa
geraTupIdBool (x:[]) (y:[]) = [(x,y)]
geraTupIdBool (x:xs) (y:ys) = (x,y): geraTupIdBool xs ys

-- gera uma lista com is ids
geraListaIds xs (VAR str) = add_lista xs str
geraListaIds xs (NAO a) = geraListaIds xs a
geraListaIds xs (E a b) = concatena (geraListaIds xs a) (geraListaIds xs b)
geraListaIds xs (OU a b) = concatena (geraListaIds xs a) (geraListaIds xs b)
geraListaIds xs (IM a b) = concatena (geraListaIds xs a) (geraListaIds xs b)
geraListaIds xs (BI a b) = concatena (geraListaIds xs a) (geraListaIds xs b)

--adiciona o elemento se ele ja não pertencer a lista
add_lista [] new = [new]
add_lista (x:xs) new = if x == new then (x:xs) else x:add_lista xs new

--concatena duas listas sem repetir os elemento que estão nas duas listas
concatena (x:xs) [] = (x:xs)
concatena (x:xs) (y:ys) = add_lista (concatena (x:xs) ys) y

--concatena duas lista mantendo as duas mesmo se os elementos repetirem
concatena_rep [] (y:ys) = (y:ys)
concatena_rep (x:xs) (y:ys) = x:concatena_rep xs (y:ys)

--retorna o valor booleano de arcordo com o id
valor_atual str ((i,b):xs) = if str == i then b else valor_atual str xs

--gera uma string com um zero para cada id distinto da expressão
geraStringBinaria (x:[]) = ['0']
geraStringBinaria (x:xs) = '0': geraStringBinaria xs

--soma um na string binaria
incrementaBin xs = inverteLista (somaStringBinaria 1 (inverteLista xs))

inverteLista (x:[]) = [x]
inverteLista (x:xs) = concatena_rep (inverteLista xs) [x]

somaStringBinaria _ [] = []
somaStringBinaria n (x:xs) = if n == 1 then if x == '1' then '0':somaStringBinaria 1 xs else '1':somaStringBinaria 0 xs else (x:xs)

--gera uma lista com booleanos n lugar dos 0s e 1s
geraListaBool [] = []
geraListaBool (x:xs) = if x == '1' then True:geraListaBool xs else False:geraListaBool xs

--verifica se todos são '1' na na String
testaUm [] = True
testaUm (x:xs) = if x == '1' then testaUm xs else False

--inicializa String Binaria
inicializaBin expr = geraStringBinaria(geraListaIds [] expr)

--ela chama com a primeira possibilidade
roraTudo expr = roraTudo' (inicializaBin expr) expr -- função principal

--roda até achar uma sulução fazendo os testes com todas as combinações possiveis
roraTudo' strbin expr = if testaUm strbin then (if (chamaTeste strbin expr)==True then mostra strbin expr else putStr "Sem Solução\n") else (if (chamaTeste strbin expr) == True then mostra strbin expr else (roraTudo'( incrementaBin strbin) expr))

--chama um teste com uma determinada expreção
chamaTeste strbin expr = resultado expr (geraTupIdBool (geraListaIds [] expr) (geraListaBool strbin))



--apenas para mostrar a solução que resolve o problema

--mostra como fica a sulução se ela existir
mostra strbin expr = putStr ("solução encontrada:\n" ++ (show (muda4Bool expr (geraTupIdBool (geraListaIds [] expr) (geraListaBool strbin)))) ++ "\nE os ids recebem os seguintes valores:\n" ++ (show ((geraTupIdBool (geraListaIds [] expr) (geraListaBool strbin)))) ++ "\n")

--gera o dado com true ou false no lugar dos ids
muda4Bool (VAR str) tuplaIdBool = C (valor_atual str tuplaIdBool)
muda4Bool (NAO a) tuplaIdBool = NAO (muda4Bool a tuplaIdBool)
muda4Bool (E a b) tuplaIdBool = E (muda4Bool a tuplaIdBool) (muda4Bool b tuplaIdBool)
muda4Bool (OU a b) tuplaIdBool = OU (muda4Bool a tuplaIdBool) (muda4Bool b tuplaIdBool)
muda4Bool (IM a b) tuplaIdBool = IM (muda4Bool a tuplaIdBool) (muda4Bool b tuplaIdBool)
muda4Bool (BI a b) tuplaIdBool = BI (muda4Bool a tuplaIdBool) (muda4Bool b tuplaIdBool)