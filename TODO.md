 * if a package has 'withFaslsFor "none"', we need to install the source directly
 * rename withFaslFor to builtFor (builtWith?), as fast-load files are not the only implementation-specific
   build product.
 * withFaslsFor should error on invalid options
 * 'name' should be 'system-name' or 'system' to make it clear it must be exactly the system name.
 * asdf-system/ -> asdf-systems/
 * Include a symlink to the source in the output? would be handy.
