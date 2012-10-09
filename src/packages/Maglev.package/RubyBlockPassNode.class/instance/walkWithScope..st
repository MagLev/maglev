*maglev-runtime
walkWithScope: aScope
    inBoot := aScope inBootstrap  .
    bodyNode walkWithScope: aScope .

