Assuming you want to add optional support for a new Apron-compatible, relational abstract domain, called LIB, you have to:
1. Declare a new optional library implementation in the dune file:
```
  (select libinst.ml from
   (lib -> libinst.real.ml)
   (-> libinst.dummy.ml))
```
2. Create an empty file `libinst.dummy.ml`
3. Write the implementation of the modules for the domains in `libinst.real.ml`, and call `Instances.register_instance` upon this module
4. `open Libinst` in `Instances_choices.ml` to make sure the instances are declared for the command-line options.
