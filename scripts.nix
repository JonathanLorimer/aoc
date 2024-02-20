{s}: 
{
  ghcidScript = s "dev" "ghcid --command 'cabal new-repl lib:aoc' --allow-eval --warnings";
  ghcidTestScript = s "dev:t" "ghcid --command 'cabal new-repl test:aoc-tests' --allow-eval --warnings";
  testScript = s "tst" "cabal run test:aoc-tests";
  hoogleScript = s "hgl" "hoogle serve";
}
