DEMO!!!
=======

This demonstrates some very basic concepts for a silly schema change.
(Basically adding a flag, and adding a reverse edge as a silly example)

The diff patches are generated automatically, but the user is responsible for
writing an "INIT" file for any new fields added.  For example, we are adding
  
 - A field to node attributes to flag as active or not (not really used)

 - A backwards reference to the edge going the other direction
     
This code is in upd_code/discovery_init

Files:
-------
 - discovery_v1.py : the first version of discovery with JSON redis nibv1  
 - discovery_v2.py : the second version of discovery with JSON redis nibv2 
 - redisnibjson_v1.py : JSON redis nibv1 (schema at top of file)
 - redisnibjson_v2.py : JSON redis nibv2 (schema at top of file)
 - upd_code/json_template_discov_v1 : describes the "templates" for JSON schema of v1 of discovery
                     (basically, this file contains one example JSON entry for each type)
 - upd_code/json_template_discov_v2 : describes the "templates" for JSON schema of v2 of discovery
 - upd_code/discovery_init : describes how to init the new fields between the two schema
 - upd_code/json_diff_to_patch.py :  this code eats the above 3 files and produces dsu.py, the update code
 - upd_code/decode.py : helper file with json unicode nastiness
 - upd_code/do_upd.py : this (non-lazily) applies the generated dsu.py patches for all redis entries

Running:
--------

1) First generate dsu.py: 
      ~/frenetic/lang/py/examples/updatedemo/upd_code$ python json_diff_to_patch.py json_template_discov_v1  json_template_discov_v2 discovery_init
   This will output dsu.py, which is a file with functions for updating any changed JSON "templates"

2) Launch webkat:
      ~/frenetic$ ./webkat.native

3) In your redis directory (wherever that is), clear any previous runs, then launch Redis:  
      ~/redis-2.8.17/src$ rm dump.rdb
      ~/redis-2.8.17/src$ ./redis-server

4) Launch mininet:
      sudo mn --topo=tree,2,3 --controller=remote --mac

5) Launch discovery v1: 
      ~/frenetic/lang/py/examples/updatedemo$ python discovery_v1.py
5b) Wait for some PROBE messages to populate (~15 seconds)

6) Just as a check, make sure Redis populates with the old schema:
      ~/frenetic/lang/py/examples$ python print_redis.py

7) Run the update:
      ~/frenetic/lang/py/examples/updatedemo/upd_code$ python do_upd.py

8) Kill discovery_v1.py, then launch v2:
      ~/frenetic/lang/py/examples/updatedemo$ python discovery_v2.py
     (Since this is not a backward-incompatible example, you won't see error
     messages in discovery_v1...we are only adding fields, not deleting anything)

9) Wait ~15 seconds; Check out how it now prints reverse edges

