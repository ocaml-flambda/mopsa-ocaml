##############################################################################
#                                                                            #
#  This file is part of MOPSA, a Modular Open Platform for Static Analysis.  #
#                                                                            #
#  Copyright (C) 2017-2019 The MOPSA Project.                                #
#                                                                            #
#  This program is free software: you can redistribute it and/or modify      #
#  it under the terms of the GNU Lesser General Public License as published  #
#  by the Free Software Foundation, either version 3 of the License, or      #
#  (at your option) any later version.                                       #
#                                                                            #
#  This program is distributed in the hope that it will be useful,           #
#  but WITHOUT ANY WARRANTY; without even the implied warranty of            #
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the             #
#  GNU Lesser General Public License for more details.                       #
#                                                                            #
#  You should have received a copy of the GNU Lesser General Public License  #
#  along with this program.  If not, see <http://www.gnu.org/licenses/>.     #
#                                                                            #
##############################################################################


# Framework packs
mopsa_analyzer.framework = core lattices sig combiners toplevel engines output config runner
mopsa_analyzer.framework.core = semantic dstack route ast print context query \
								avalue alarm token effect lattice flow cases \
								eval post id manager hook cache all
mopsa_analyzer.framework.core.ast = program frontend typ operator constant var expr stmt visitor
mopsa_analyzer.framework.lattices = partial_map partial_inversible_map_sig \
		     partial_inversible_map powerset_sig powerset \
		     powersetwithunder pointwise pair
mopsa_analyzer.framework.sig = abstraction combiner reduction
mopsa_analyzer.framework.sig.abstraction = stacked stacked_functor \
			    domain functor \
			    simplified simplified_functor \
			    stateless \
			    value value_functor simplified_value
mopsa_analyzer.framework.sig.combiner = stacked stacked_functor \
			 domain functor \
			 simplified simplified_functor \
			 stateless
mopsa_analyzer.framework.sig.reduction = eval exec simplified value

mopsa_analyzer.framework.combiners = common value domain
mopsa_analyzer.framework.combiners.value = nonrel union product
mopsa_analyzer.framework.combiners.domain = switch compose product simplified_product stateless_switch
mopsa_analyzer.framework.abstraction.json = syntax visitor parser
mopsa_analyzer.framework.engines = engine automatic interactive dap
mopsa_analyzer.framework.output = common text json factory
mopsa_analyzer.framework.config = abstraction paths options

mopsa_analyzer.framework.config.abstraction = syntax visitor parser builder


# Packages
PKGS += str yojson gmp apron apron.boxMPQ apron.octMPQ apron.polkaMPQ zarith threads


# MOPSA libraries
MOPSALIBS += utils:MopsaUtils

# this is needed for ocamlrun to find our dll*.so when running the bytecode bynary
# TODO: find a better way
DLLPATHS += -dllpath `pwd`/$(MOPSAROOT)/utils/lib
