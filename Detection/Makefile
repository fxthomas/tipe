all: grid.cmo sortarray.cmo util.cmo progressgraph.cmo bitmap.cmo colorspace.cmo channel.cmo graylevel.cmo convolution.cmo intensitymap.cmo raytrace.cmo edgedetect.cmo cclabel.cmo speed_filter.cmo locator.cmo locate_cclabel.cma locate_circle.cma geom.cmo

grid.cmi: grid.mli
	ocamlc -c grid.mli

grid.cmo: grid.ml grid.cmi
	ocamlc -c grid.ml

sortarray.cmi: sortarray.mli
	ocamlc -c sortarray.mli

sortarray.cmo: sortarray.ml sortarray.cmi
	ocamlc -c sortarray.ml

util.cmi: util.mli
	ocamlc -c util.mli

util.cmo: util.ml util.cmi
	ocamlc -c util.ml

progressgraph.cmi: progressgraph.mli bitmap.cmo
	ocamlc -c progressgraph.mli

progressgraph.cmo: progressgraph.cmi progressgraph.ml bitmap.cmo
	ocamlc -c progressgraph.ml

bitmap.cmi: bitmap.mli
	ocamlc -c bitmap.mli

bitmap.cmo: bitmap.ml bitmap.cmi
	ocamlc -c bitmap.ml

colorspace.cmi: colorspace.mli
	ocamlc -c colorspace.mli

colorspace.cmo: colorspace.ml colorspace.cmi
	ocamlc -c colorspace.ml

channel.cmi: channel.mli colorspace.cmo
	ocamlc -c channel.mli

channel.cmo: channel.ml channel.cmi colorspace.cmo
	ocamlc -c channel.ml

graylevel.cmi: graylevel.mli progressgraph.cmo colorspace.cmo channel.cmo
	ocamlc -c graylevel.mli

graylevel.cmo: graylevel.ml graylevel.cmi progressgraph.cmo colorspace.cmo channel.cmo
	ocamlc -c graylevel.ml

convolution.cmi: convolution.mli channel.cmo progressgraph.cmo
	ocamlc -c convolution.mli

convolution.cmo: convolution.ml convolution.cmi channel.cmo progressgraph.cmo
	ocamlc -c convolution.ml

edgedetect.cmi: edgedetect.mli progressgraph.cmo util.cmo bitmap.cmo colorspace.cmo channel.cmo graylevel.cmo convolution.cmo intensitymap.cmo
	ocamlc -c edgedetect.mli

edgedetect.cmo: edgedetect.ml edgedetect.cmi progressgraph.cmo util.cmo bitmap.cmo colorspace.cmo channel.cmo graylevel.cmo convolution.cmo intensitymap.cmo
	ocamlc -c edgedetect.ml

intensitymap.cmi: intensitymap.mli channel.cmo colorspace.cmo
	ocamlc -c intensitymap.mli

intensitymap.cmo: intensitymap.ml intensitymap.cmi channel.cmo colorspace.cmo
	ocamlc -c intensitymap.ml

raytrace.cmi: raytrace.mli
	ocamlc -c raytrace.mli

raytrace.cmo: raytrace.ml raytrace.cmi
	ocamlc -c raytrace.ml

cclabel.cmi: cclabel.mli sortarray.cmo util.cmo colorspace.cmo progressgraph.cmo
	ocamlc -c cclabel.mli

cclabel.cmo: cclabel.ml cclabel.cmi sortarray.cmo util.cmo colorspace.cmo progressgraph.cmo
	ocamlc -c cclabel.ml

speed_filter.cmi: util.cmo bitmap.cmo colorspace.cmo progressgraph.cmo channel.cmo graylevel.cmo speed_filter.mli
	ocamlc -c speed_filter.mli

speed_filter.cmo: speed_filter.ml speed_filter.cmi util.cmo bitmap.cmo colorspace.cmo progressgraph.cmo channel.cmo graylevel.cmo
	ocamlc -c speed_filter.ml

locator.cmi: locator.mli grid.cmo sortarray.cmo progressgraph.cmo util.cmo bitmap.cmo colorspace.cmo channel.cmo cclabel.cmo speed_filter.cmo
	ocamlc -c locator.mli

locator.cmo: locator.ml locator.cmi grid.cmo sortarray.cmo progressgraph.cmo util.cmo bitmap.cmo colorspace.cmo channel.cmo cclabel.cmo speed_filter.cmo
	ocamlc -c locator.ml

locate_circle.cma: edgedetect.cmo intensitymap.cmo raytrace.cmo progressgraph.cmo util.cmo colorspace.cmo channel.cmo graylevel.cmo convolution.cmo
	ocamlc -a -o locate_circle.cma progressgraph.cmo util.cmo colorspace.cmo channel.cmo graylevel.cmo convolution.cmo  intensitymap.cmo edgedetect.cmo raytrace.cmo

locate_cclabel.cma: grid.cmo sortarray.cmo progressgraph.cmo util.cmo bitmap.cmo colorspace.cmo channel.cmo cclabel.cmo locator.cmo speed_filter.cmo
	ocamlc -a -o locate_cclabel.cma grid.cmo sortarray.cmo progressgraph.cmo util.cmo colorspace.cmo channel.cmo cclabel.cmo speed_filter.cmo locator.cmo

geom.cmi: geom.mli
	ocamlc -c geom.mli

geom.cmo: geom.cmi geom.ml
	ocamlc -c geom.ml

clean:
	rm *~ || echo "No backup files"
	rm *.cmi || echo "No compiled interface files"
	rm *.cmo || echo "No byte-compiled modules"
	rm *.cma || echo "No libraries"

clean-all: clean
	rm *.mli || echo "No CAML interface files"