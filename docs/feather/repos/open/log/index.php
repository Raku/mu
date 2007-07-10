<?php
/**
 * Repos Style log reader (c) 2007  www.reposstyle.com
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *    http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
*/

// === Print svn log --xml to response ===
$repo = '@@Repository@@'; // repository root, no trailing slash

// === configuration done ===
if (strstr($repo,'@@')) die('The log script must be configured with a root URL');

if (!isset($_REQUEST['target'])) die("Parameter 'target' is required");
$url = $repo . $_REQUEST['target'];

$cmd = "svn log --xml --verbose --incremental --non-interactive \"$url\" ";
 
header('Content-Type: text/xml');
echo('<?xml version="1.0"?>
<?xml-stylesheet type="text/xsl" href="/repos/view/log.xsl"?>
<log>
');
passthru($cmd);
echo('</log>
');
?>
