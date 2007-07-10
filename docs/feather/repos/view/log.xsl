<?xml version="1.0"?>
<!--
Repos Style (c) 2004-2007 Staffan Olsson www.reposstyle.com

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

  ==== Repos Subversion log layout ====
  Note that browser transformations only work if the
  stylesheet is read from the same domain as the XML
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns="http://www.w3.org/1999/xhtml" version="1.0">
	<xsl:output method="html" encoding="UTF-8" omit-xml-declaration="no" indent="no"
		doctype-public="-//W3C//DTD XHTML 1.0 Strict//EN"/>
	<xsl:param name="reposstyle-version">1.1</xsl:param>
	
	<!-- === repos style configuration === -->
	<!-- static: absolute url to style application -->
	<xsl:param name="static">/repos/</xsl:param>
	<!-- cssUrl: absolute url to css folder -->
	<xsl:param name="cssUrl"><xsl:value-of select="$static"/>style/</xsl:param>
	<!-- log viewer does not know the repository URL -->
	<xsl:param name="repoUrl">javascript:history.go(-1)</xsl:param>
	<!-- ===== end of configuration ===== -->
	
	<xsl:param name="spacer" select="' &#160; '"/>
	<xsl:template match="/">
		<html xmlns="http://www.w3.org/1999/xhtml">
			<head>
				<title>
					<xsl:text>repos: history</xsl:text>
				</title>
				<meta http-equiv="Content-Type" content="text/html; charset=utf-8"/>
				<!-- if search crawlers has access, contents should not be cached -->
				<meta name="robots" content="noarchive"/>
				<link rel="shortcut icon" href="/favicon.ico"/>
				<link title="repos" rel="stylesheet" type="text/css" href="{$cssUrl}global.css"/>
				<link title="repos" rel="stylesheet" type="text/css" href="{$cssUrl}log/log.css"/>
			</head>
			<body class="log xml">
				<xsl:apply-templates select="log"/>
			</body>
		</html>
	</xsl:template>
	<xsl:template match="svn">
		<xsl:apply-templates select="index"/>
	</xsl:template>
	<xsl:template match="log">
		<xsl:call-template name="commandbar"/>
		<xsl:call-template name="contents"/>
		<xsl:call-template name="footer"/>
	</xsl:template>
	<xsl:template name="commandbar">
		<div id="commandbar">
		<a id="repository" href="{$repoUrl}">return to repository</a>
		</div>
	</xsl:template>
	<xsl:template name="contents">
		<h1>Repository history</h1>
		<xsl:apply-templates select="logentry"/>
		<p><a class="action" href="{$repoUrl}">&#171; return to repository</a></p>
	</xsl:template>
	<xsl:template name="footer">
		<div id="footer">
		<span><a href="http://www.reposstyle.com/" target="_blank">Repos&#160;Style</a>&#160;<xsl:value-of select="$reposstyle-version"/> 
		&amp; <a href="http://www.kde-look.org/content/show.php?content=16479" target="_blank">Cezanne&#160;icons</a></span>
		<span id="badges">
		</span>
		<span class="legal">Powered by Subversion</span>
		</div>
	</xsl:template>
	<xsl:template match="logentry">
		<xsl:param name="n" select="position() - 1"/>
		<div id="rev{@revision}" class="logentry n{$n mod 4}">
			<h3>
				<span class="revision" title="the changeset number (version number)">
					<xsl:value-of select="@revision"/>
				</span>
				<span id="author:{@revision}" class="username" title="author">
					<xsl:value-of select="author"/>
				</span>
				<xsl:value-of select="$spacer"/>
				<span id="datetime:{@revision}" class="datetime" title="date and time of the commit">
					<xsl:value-of select="date"/>
				</span>
				<xsl:value-of select="$spacer"/>
			</h3>
			<xsl:if test="string-length(msg) > 0">
				<div id="message:{@revision}" class="message" title="Log message">
						<xsl:call-template name="linebreak">
							<xsl:with-param name="text" select="msg"/>
						</xsl:call-template>
				</div>
			</xsl:if>
			<xsl:apply-templates select="paths">
				<xsl:with-param name="fromrev" select="following-sibling::*[1]/@revision"/>
			</xsl:apply-templates>
		</div>
	</xsl:template>
	<xsl:template match="paths">
		<xsl:param name="fromrev"/>
		<xsl:apply-templates select="path">
			<xsl:with-param name="fromrev" select="$fromrev"/>
			<xsl:sort select="." order="ascending"/>
		</xsl:apply-templates>
	</xsl:template>
	<xsl:template match="paths/path">
		<xsl:param name="fromrev"/>
		<xsl:param name="pathid">
			<xsl:call-template name="getFileID">
				<xsl:with-param name="filename" select="."/>
			</xsl:call-template>
			<xsl:value-of select="'-'"/>
			<xsl:value-of select="../../@revision"/>
			<xsl:value-of select="@action"/>
		</xsl:param>
		<div class="row log-{@action}">
			<xsl:if test="@action='A'">
				<span class="path" title="Added {.}">
					<xsl:value-of select="."/>
				</span>
				<xsl:value-of select="$spacer"/>
				<xsl:if test="@copyfrom-path">
					<span class="copied" title="Copied from {@copyfrom-path} version {@copyfrom-rev}">
						<span class="folder">
							<xsl:value-of select="@copyfrom-path"/>&#160;</span>
						<span class="revision">
							<xsl:value-of select="@copyfrom-rev"/>
						</span>
					</span>
					<xsl:value-of select="$spacer"/>
				</xsl:if>
			</xsl:if>
			<xsl:if test="@action='D'">
				<span class="path" title="Deleted {.}, so it only exists in versions prior to {../../@revision}.">
					<xsl:value-of select="."/>
				</span>
				<xsl:value-of select="$spacer"/>
			</xsl:if>
			<xsl:if test="@action='M'">
				<span class="path" title="Modified {.}">
					<xsl:value-of select="."/>
				</span>
				<xsl:value-of select="$spacer"/>
			</xsl:if>
		</div>
	</xsl:template>
	<xsl:template name="linebreak">
		<xsl:param name="text"/>
		<xsl:choose>
			<xsl:when test="contains($text, '&#10;')">
				<xsl:value-of select="substring-before($text, '&#10;')"/>
				<br/>
				<xsl:call-template name="linebreak">
					<xsl:with-param name="text" select="substring-after($text, '&#10;')"/>
				</xsl:call-template>
			</xsl:when>
			<xsl:otherwise>
				<xsl:value-of select="$text"/>
			</xsl:otherwise>
		</xsl:choose>
	</xsl:template>
	<xsl:template name="getFileID">
		<xsl:param name="filename" select="@href"/>
		<xsl:value-of select="translate($filename,'%/()@&amp;+=,~$! ','_____________')"/>
	</xsl:template>
</xsl:stylesheet>
