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
framework = core lattices sig combiners toplevel engines output config runner
framework.core = semantic route ast print query avalue context alarm token log \
		 lattice flow cases eval post id manager hook cache all
framework.core.ast = program frontend typ operator constant var expr stmt visitor
framework.lattices = partial_map partial_inversible_map_sig \
		     partial_inversible_map powerset_sig powerset \
		     powersetwithunder pointwise pair
framework.sig = abstraction combiner reduction
framework.sig.abstraction = stacked stacked_functor \
			    domain functor \
			    simplified simplified_functor \
			    stateless \
			    value value_functor simplified_value
framework.sig.combiner = stacked stacked_functor \
			 domain functor \
			 simplified simplified_functor \
			 stateless
framework.sig.reduction = eval exec simplified value
framework.combiners = common value domain
framework.combiners.value = nonrel union product
framework.combiners.domain = switch compose product simplified_product stateless_switch
framework.abstraction.json = syntax visitor parser
framework.engines = engine automatic interactive
framework.output = common text json factory
framework.config = abstraction paths options
framework.config.abstraction = syntax visitor parser builder


# Packages
PKGS += str yojson gmp apron apron.boxMPQ apron.octMPQ apron.polkaMPQ zarith threads


# MOPSA libraries
MOPSALIBS += utils:utils

# this is needed for ocamlrun to find our dll*.so when running the bytecode bynary
# TODO: find a better way
DLLPATHS += -dllpath `pwd`/$(MOPSAROOT)/utils/lib
