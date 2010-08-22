signature ENCODING = 
sig

structure TF: TEXT_FRAG

datatype t = 
	 None
       | GZip
       | Compress
       | Deflate
       | Identity
       | Other of string

val format: t -> TF.t

end
