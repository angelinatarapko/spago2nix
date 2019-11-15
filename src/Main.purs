module Main where

import Prelude

import Control.Alt ((<|>))
import Core (DhallExpr(..), buildScript, exit, runCommand, runDhallToJSON)
import Data.Either (Either(..))
import Data.List (List, (:), intercalate)
import Data.List as List
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Class.Console (log)
import Generate as Generate
import Simple.JSON as JSON
import Data.Array as Array
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Options.Applicative ((<**>), Parser, ParserInfo, argument, command, commandGroup, execParser, helper, hidden, idm, info, many, metavar, progDesc, str, subparser)

data Command
  = GenerateCommand GenerateOptions
  | InstallCommand (Array String)
  | BuildCommand (Array String)
  | BuildNixCommand (Array String)

derive instance commandEq :: Eq Command
derive instance genericCommand :: Generic Command _
instance showCommand :: Show Command where show = genericShow

hello :: Parser Command
hello = Hello <<< Array.fromFoldable <$> many (argument str (metavar "TARGET..."))

installCommandParser :: Parser Command
installCommandParser = InstallCommand <<< Array.fromFoldable <$> many (argument str (metavar "[passthrough args for nix-shell]"))

command :: Parser Command
command = subparser
  ( command "install"
    (info installCommandParser
    (progDesc "Install dependencies from spago-packages.nix in Spago style"))
  <> command "goodbye"
      (info (pure Goodbye)
            (progDesc "Say goodbye"))
    )
  <|> subparser
    ( command "bonjour"
      (info hello
            (progDesc "Print greeting"))
  <> command "au-revoir"
      (info (pure Goodbye)
            (progDesc "Say goodbye"))
  <> commandGroup "French commands:"
  <> hidden
    )

run :: Command -> Effect Unit
run (Hello targets) = log $ "Hello, " <> intercalate ", " targets <> "!"
run Goodbye = log "Goodbye."

opts :: ParserInfo Command
opts = info (command <**> helper) idm

main :: Effect Unit
main = execParser opts >>= run

main :: Effect Unit
main = Aff.launchAff_ do
  case args of
    "generate" : rest -> Generate.generate
    "install" : rest -> install rest
    "build" : rest -> build SpagoStyle rest
    "build-nix" : rest -> build NixStyle rest
    "help" : rest -> log help
    List.Nil -> log help
    _ -> do
      log $ "Unknown arguments: " <> List.intercalate " " args

install :: List String -> Aff Unit
install extraArgs = do
  buildScript { attr: "installSpagoStyle", path: installPath, extraArgs }
  runCommand { cmd: "bash", args: [installPath] }
  log $ "Wrote install script to " <> installPath
  exit 0
  where
    installPath = ".spago2nix/install"

data BuildStyle
  = SpagoStyle
  | NixStyle

build :: BuildStyle -> List String -> Aff Unit
build buildStyle extraArgs = do
  buildScript { attr: buildStyleAttr, path: buildPath, extraArgs }
  json <- runDhallToJSON (DhallExpr "(./spago.dhall).sources") <|> pure ""
  globs <- case JSON.readJSON json of
    Left _ -> do
      let defaultGlob = "src/**/*.purs"
      log $ "failed to read sources from spago.dhall using dhall-to-json."
      log $ "using default glob: " <> defaultGlob
      pure [defaultGlob]
    Right (xs :: Array String) -> do
      log $ "using sources from spago.dhall: " <> show xs
      pure xs
  runCommand { cmd: "bash", args: [buildPath] <> globs }
  log $ "Wrote build script to " <> buildPath
  exit 0
  where
    buildPath = ".spago2nix/build"
    buildStyleAttr = case buildStyle of
      SpagoStyle -> "buildSpagoStyle"
      NixStyle -> "buildFromNixStore"

help :: String
help = """spago2nix - generate Nix derivations from packages required in a spago project, and allow for installing them and building them.

  Usage: spago2nix (generate | install | build)

Available commands:
  generate [--project-dir PROJECT-DIR] [--output OUTPUT] [--cache-dir CACHE-DIR]
    Generate a Nix expression of packages from Spago

    Optional arguments:
      PROJECT-DIR    The directory with ./spago.dhall and ./packages.dhall file (default: $CWD)
      OUTPUT         The output file to generate (default: ./spago-packages.nix)
      CACHE-DIR      The cache dir to generate (default: ./.spago2nix/)

  install [passthrough args for nix-shell]
    Install dependencies from spago-packages.nix in Spago style
  build [passthrough args for nix-shell]
    Build the project Spago style
  build-nix [passthrough args for nix-shell]
    Build the project using dependency sources from Nix store
"""
