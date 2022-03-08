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



# Python
mopsa_analyzer.lang += python
mopsa_analyzer.lang.python = ast pp addr alarms utils visitor operators  ast_compare hooks frontend   \
              soundness objects desugar libs  data_model types flows program packing
mopsa_analyzer.lang.python.objects = lambda class data_container_utils function py_list range tuple dict constant_dict dict_reduction py_set py_object iterable
mopsa_analyzer.lang.python.desugar = assert bool comprehensions if import iterable_assign loops with
mopsa_analyzer.lang.python.libs = math py_mopsa py_std py_unittest py_typing
mopsa_analyzer.lang.python.data_model = callable arith_ops compare_ops aug_assign	\
                         attribute subscript
mopsa_analyzer.lang.python.types = nominal_types addr_env structural_types polymorphism t_int t_float t_string  t_complex dummy_numeric type_annot
mopsa_analyzer.lang.python.packing = static_scope


# MOPSA libraries
MOPSALIBS += parsers/python:MopsaPythonParser
