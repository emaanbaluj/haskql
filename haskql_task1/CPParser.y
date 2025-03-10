{
module CPParser where
import CPLexer
}

%name parseCP
%tokentype { PosnToken }
%error { parseError }

%token
    cp     { PT _ TokenCP }
    csv    { PT _ (TokenCSV $$) }

%%

Command : cp csv csv { ("cp", $2, $3) } 

{
parseError :: [PosnToken] -> a
parseError toks = error ("Parse error at " ++ unwords (map tokenPosn toks))
}