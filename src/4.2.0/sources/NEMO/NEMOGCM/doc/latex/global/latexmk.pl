## Defaults
#$silent   = 1;
$pdf_mode = 5;

## Using relative paths
$ENV{'openout_any'} = 'a'       ;
$do_cd              = 1         ;
$out_dir            = '../build';

## Custom cmds
set_tex_cmds('-shell-escape -file-line-error -interaction=nonstopmode');
#set_tex_cmds('-shell-escape -file-line-error');
$makeindex = "makeindex %O -s ../../global/index.ist -o $out_dir/%D $out_dir/%S";
## %D: Destination file (.ind for index)
## %O: Options
## %R: Root filename (${model}_manual)
## %S: Source file      (.idx for index)
