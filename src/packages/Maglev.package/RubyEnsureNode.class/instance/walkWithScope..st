*maglev-runtime
walkWithScope: aScope
  isStEnsure := aScope inBootstrap .
  ^ super  walkWithScope: aScope .

