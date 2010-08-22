structure StringKey =
struct

type hash_key = string
fun hashVal s = HashString.hashString s
fun sameKey(k1,k2) = String.compare(k1,k2) = EQUAL
		     
end

structure StringMap = HashTableFn(StringKey)
