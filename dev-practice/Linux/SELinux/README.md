# SELinux

This is a collection of my notes regarding SELinux.

[SELinux](https://github.com/SELinuxProject) is a notoriously complex security enhancement application specifically intended for
the Linux kernel, with GNU/Linux userspace utilities.

<table>
  <tr>
    <th colspan=2><div align="center">Useful Notes</div></th>
  </tr>
  <tr>
    <td colspan=2>SELinux has its own "raw" language to specify policy data. But it uses M4 macros to simplify the underlying language.</td>
  </tr>
  <tr>
    <td><code>$ macro-expander</code></td>
    <td>This is a useful userspace utility to find out what a specific SELinux M4 macro expands to.</td>
  </tr>
  <tr>
    <td><code>$ checkmodule</code></td>
    <td>This is the command used to check the syntax of an SELinux policy module from a <code>.te</code> file.<br>Example: <code>checkmodule -M -m -o my-module.mod my-policy.te</code><br><b>NOTE</b>: This is only used in low-level SELinux policies. It is highly recommended to use M4 macros and the SELinux-provided Makefile instead.</td>
  </tr>
  <tr>
    <td><code>$ semodule_package</code></td>
    <td>This is the command used to package an SELinux module produced by <code>checkmodule</code>.<br>Example: <code>semodule_package -o my-module.pp -m my-module.mod</code><br><b>NOTE</b>: This is only used in low-level SELinux policies. It is highly recommended to use M4 macros and the SELinux-provided Makefile instead.</td>
  </tr>
  <tr>
    <td colspan=2><code>/usr/share/selinux/devel/Makefile</code> is the file that should be used (with GNU <code>make</code>) when producing a policy package that may or may not use M4 macros.<br>Example: <code>make -f /usr/share/selinux/devel/Makefile my-policy.pp</code><br>It is assumed that <code>my-policy.te</code>, <code>my-policy.fc</code>, and <code>my-policy.if</code> all exist.</td>
  </tr>
</table>

