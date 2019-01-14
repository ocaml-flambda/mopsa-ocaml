<?php
header("Content-Type: application/json; charset=UTF-8");

include_once 'config.php';

$basetmp = tempnam(sys_get_temp_dir(), 'mopsa');
$tmp = $basetmp . '.' . $_POST["name"];
rename($basetmp, $tmp);

file_put_contents($tmp, $_POST["content"]);

$mopsa = $mopsa_root . "/bin/mopsa-" . $_POST["lang"];
$command = $mopsa . " -format=json " . $tmp;

exec($command, $out);

echo join(' ', $out);
?>
