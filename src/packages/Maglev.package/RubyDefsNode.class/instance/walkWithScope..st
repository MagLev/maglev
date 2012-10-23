as yet unclassified
walkWithScope: aScope
	"receiver belongs to current scope, the method defn gets a new scope"
   receiverNode walkWithScope: aScope .
   ^ super walkWithScope: aScope .

