module Options
  ( parseCmd
  , ProofOpts(..)
  , ProofAction(..)
  , BackEnd(..)
  , FrontEnd(..)
  , Cmd(..)
  )
where

import           Options.Applicative
import           Options.Applicative.Help.Pretty
import           Data.Semigroup                 ( (<>) )
import           Util.Cfg

data ProofAction = EmitR1cs
                 | Setup
                 | Prove
                 deriving (Eq, Show)

proofActionP :: Parser ProofAction
proofActionP =
  flag' EmitR1cs (long "emit-r1cs" <> help "Emit R1CS")
    <|> flag' Setup (long "setup" <> help "Setup the SNARK (emit pk, vk)")
    <|> flag' Prove (long "prove" <> help "Write the SNARK (emit x, w, pf)")

data BackEnd = Solve
             | Proof ProofOpts ProofAction
             deriving (Show)

backEndP :: Parser BackEnd
backEndP =
  flag' Solve (long "solve" <> help "Find a satisfying assignment")
    <|> (Proof <$> proofOptsP <*> proofActionP)

data FrontEnd = C String FilePath Bool
              | Zokrates String FilePath
              deriving (Show)

cP :: Parser FrontEnd
cP =
  C
    <$> strArgument (metavar "FN-NAME" <> help "C function to compile")
    <*> strArgument (metavar "C-PATH" <> help "C file to compile")
    <*> switch (long "check" <> help "Compile assertions")

zokratesP :: Parser FrontEnd
zokratesP =
  Zokrates
    <$> strArgument (metavar "FN-NAME" <> help "Zokrates function to compile")
    <*> strArgument (metavar "PATH" <> help "Zokrates file to compile")

frontEndP :: Parser FrontEnd
frontEndP = hsubparser
  (  command "c" (info cP (progDesc "C compiler front-end"))
  <> command
       "zokrates"
       (info zokratesP (progDesc "ZoKrates compiler front-end"))
  )

data Cmd = Composite (Maybe FilePath) FrontEnd BackEnd
         | Verify ProofOpts
         | Circom FilePath (Maybe FilePath) ProofOpts ProofAction
         | CCheckProve String FilePath ProofOpts
         deriving (Show)

cmdP =
  (   Composite
    <$> optional
          (strOption
            (long "inputs" <> short 'i' <> metavar "PATH" <> help
              "Inputs to use for witness computation"
            )
          )
    <*> frontEndP
    <*> backEndP
    )
    <|> hsubparser
          (  command "verify"
                     (info (Verify <$> proofOptsP) (progDesc "Verify a proof"))
          <> command
               "circom"
               (info
                 (   Circom
                 <$> strArgument (metavar "PATH" <> help "Circom file")
                 <*> optional
                       (strOption
                         (  long "inputs"
                         <> short 'i'
                         <> metavar "PATH"
                         <> help "Inputs to use for witness computation"
                         )
                       )
                 <*> proofOptsP
                 <*> proofActionP
                 )
                 (progDesc "Circom compilation pipeline")
               )
          <> command
               "c-check-prove"
               (info
                 (   CCheckProve
                 <$> strArgument (metavar "FN-NAME" <> help "C function")
                 <*> strArgument (metavar "PATH" <> help "C file")
                 <*> proofOptsP
                 )
                 (progDesc "Find bug and prove it")
               )
          )


data ProofOpts = ProofOpts
  { r1csPath :: FilePath
  , asJson   :: Bool
  , vkPath   :: FilePath
  , pkPath   :: FilePath
  , xPath    :: FilePath
  , wPath    :: FilePath
  , pfPath   :: FilePath
  , libPath  :: FilePath
  }
  deriving Show

proofOptsP :: Parser ProofOpts
proofOptsP =
  ProofOpts
    <$> strOption
          (  long "r1cs-path"
          <> short 'C'
          <> metavar "PATH"
          <> help "Location for the R1CS"
          <> value "C"
          )
    <*> switch
          (long "as-json" <> help "Whether to emit JSON (default is textual)")
    <*> strOption
          (  long "vk-path"
          <> short 'V'
          <> metavar "PATH"
          <> help "Location for the verification key"
          <> value "vk"
          )
    <*> strOption
          (  long "pk-path"
          <> short 'P'
          <> metavar "PATH"
          <> help "Location for the proving key"
          <> value "pk"
          )
    <*> strOption
          (  long "x-path"
          <> short 'x'
          <> metavar "PATH"
          <> help "Location for the explicit inputs"
          <> value "x"
          )
    <*> strOption
          (  long "w-path"
          <> short 'w'
          <> metavar "PATH"
          <> help "Location for the existential (private) inputs"
          <> value "w"
          )
    <*> strOption
          (  long "pf-path"
          <> short 'p'
          <> metavar "PATH"
          <> help "Location for the proof"
          <> value "pf"
          )
    <*> strOption
          (  long "libsnark-path"
          <> short 'l'
          <> metavar "PATH"
          <> help "Location of the libsnark front-end binary"
          <> value "libsnark-frontend/build/src/main"
          )

cfgHelpDoc :: Doc
cfgHelpDoc = string "Configuration Options:" <$$> indent 2 optDocs
 where
  optDoc :: CfgOption -> Doc
  optDoc o = fill l n <+> align (vsep d)
   where
    ifNonNull :: String -> [Doc]
    ifNonNull s = if null s then [] else [fillSep $ map text $ words s]
    n = string $ "--" ++ optName o
    d =
      ifNonNull (optDesc o)
        ++ ifNonNull (optDetail o)
        ++ ifNonNull ("Default: " ++ optDefault o)
        ++ [string ""]

  l :: Int
  l       = foldr max 0 $ map ((+ 2) . length . optName) options

  optDocs = vcat $ map optDoc options

parseCmd :: [String] -> IO Cmd
parseCmd args = handleParseResult $ execParserPure
  (prefs idm)
  (info
    (cmdP <**> helper)
    (  fullDesc
    <> progDesc "Compile an EQC"
    <> header "circ - the existentially quantified CIRcuit Compiler"
    <> footerDoc (Just cfgHelpDoc)
    )
  )
  args
