{ system
  , compiler
  , flags
  , pkgs
  , hsPkgs
  , pkgconfPkgs
  , errorHandler
  , config
  , ... }:
  {
    flags = {};
    package = {
      specVersion = "1.10";
      identifier = { name = "tracer-transformers"; version = "0.1.0.1"; };
      license = "Apache-2.0";
      copyright = "2019 IOHK";
      maintainer = "operations@iohk.io";
      author = "Neil Davies, Alexander Diemand, Andreas Triantafyllos";
      homepage = "";
      url = "";
      synopsis = "tracer transformers and examples showing their use";
      description = "";
      buildType = "Simple";
      isLocal = true;
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."safe-exceptions" or (errorHandler.buildDepError "safe-exceptions"))
          ] ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).lt "8.5") (hsPkgs."contravariant" or (errorHandler.buildDepError "contravariant"));
        buildable = true;
        };
      exes = {
        "tracer-transfomers-example1" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."tracer-transformers" or (errorHandler.buildDepError "tracer-transformers"))
            ];
          buildable = true;
          };
        "tracer-transfomers-example2" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."tracer-transformers" or (errorHandler.buildDepError "tracer-transformers"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/input-output-hk/iohk-monitoring-framework";
      rev = "42778ad2041dd9ba236a60c35e1f936fcc1339ae";
      sha256 = "0vn14csx376dfg5m2jc9c68qgvic9i4737qz672q5y6xy1g1lcaz";
      });
    postUnpack = "sourceRoot+=/tracer-transformers; echo source root reset to \$sourceRoot";
    }