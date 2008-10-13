#!/usr/sbin/dtrace -s
/*
 * This script uses dtrace and the Mac OS X Leopard ruby dtrace provider to
 * count the number of times each ruby method is called.  To use the
 * script (On OSX 10.5), make sure rubymethods.d is on your PATH, and then:
 *
 *     $ sudo rubymethods.d -c "my_ruby_script.rb param1 param2..."
 *                   Class                    Method    Count
 *     -------------------------------------------------------
 *                    Array                         -        1
 *                    Array                       []=        1
 *                    Array                    concat        1
 *
 *     ...
 *                   String                      to_i      994
 *                   Module              method_added     1331
 *                   Object                       dup     1516
 *                   Object                      hash     1826
 *                   Object     instance_variable_set     2624
 *                   Object                       ===     4505
 *                   Object                        ==     4546
 *                   Module                       ===     6782
 *
 */

#pragma D option quiet

ruby$target:::function-entry
{
  /*
   * Count the number of times each function is called.
   * Index by class (arg0) and method (arg1).
   */
  @count_table[copyinstr(arg0), copyinstr(arg1)]=count();
}

dtrace:::END
{
  printf("%25s %25s %8s\n", "Class", "Method", "Count");
  printf("------------------------------------------------------------\n");
  printa("%25s %25s %@8d\n", @count_table);
}



