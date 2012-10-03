*maglev-runtime
walkWithScope: aScope
  super walkWithScope: aScope .
  firstNode postWalkForYield .

