OCAML = ocamlc
FLAGS = -g

all: max eval last sumlist sigma crazy2val iter

max: max.ml
	$(OCAML) $(FLAGS) -o max max.ml

s2q: s2q.ml
	$(OCAML) $(FLAGS) -o s2q s2q.ml

check: checkmetro.ml
	$(OCAML) $(FLAGS) -o checkmetro checkmetro.ml

eval: eval.ml
	$(OCAML) $(FLAGS) -o eval eval.ml

priority: priority.ml
	$(OCAML) $(FLAGS) -o priority priority.ml

last: last.ml
	$(OCAML) $(FLAGS) -o last last.ml

sumlist: sumlist.ml
	$(OCAML) $(FLAGS) -o sumlist sumlist.ml

sigma: sigma.ml
	$(OCAML) $(FLAGS) -o sigma sigma.ml

logic: logic.ml
	$(OCAML) $(FLAGS) -o logic logic.ml

crazy3val: crazy3val.ml
	$(OCAML) $(FLAGS) -o crazy3val crazy3val.ml

clean: 
	rm -f max eval last sumlist sigma logic checkmetro crazy3val priority crazy2val iter parenize mathemadiga crs2q *.cmi *.cmo 

iter: iter.ml
	$(OCAML) $(FLAGS) -o iter iter.ml

parenize: parenize.ml
	$(OCAML) $(FLAGS) -o parenize parenize.ml

math: mathemadiga.ml
	$(OCAML) $(FLAGS) -o mathemadiga mathemadiga.ml