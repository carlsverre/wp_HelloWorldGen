<?php
/*
Plugin Name: Hello World Code Generator
Plugin URI: http://thelab.carlsverre.com
Description: This program will output the hello world
program in a random language.
Version: 0.1
Author: Carl Sverre
Author URI: http://www.carlsverre.com
 */

/*  Copyright 2009 CARL SVERRE (email : carl@carlsverre.com)

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 */

// CONFIG
global $helloworld_table_name;
$helloworld_table_name = "helloworldsnippets";

function helloworld_install() {
	global $wpdb, $helloworld_table_name;

	$max_lines = 50;
	
	$table_name = $wpdb->prefix . $helloworld_table_name;
	
	$table_sql = "CREATE TABLE $table_name (
					id mediumint(9) NOT NULL AUTO_INCREMENT,
					title tinytext NOT NULL,
					code text NOT NULL,
					UNIQUE KEY id (id)
				);";
	
	$row_sql = "INSERT INTO $table_name (title, code) VALUES(%s, %s)";

	if($wpdb->get_var("SHOW TABLES LIKE '$table_name'") != $table_name) {
		require_once(ABSPATH . 'wp-admin/includes/upgrade.php');
		dbDelta($table_sql);
		
		require_once(dirname(__FILE__).'/helloworld.php');
		
		foreach($snippets as $snippet) {
			if(count(explode("\n", $snippet['code'])) > $max_lines)
			   continue;
			$results = $wpdb->query(
				$wpdb->prepare($row_sql,
					$snippet['title'],
					$snippet['code']
				)
			); 
		}
	}
} //end install

register_activation_hook(__FILE__, 'helloworld_install');

function helloworld_uninstall() {
	global $wpdb, $helloworld_table_name;
	
	$table_name = $wpdb->prefix . $helloworld_table_name;

	$drop_sql = "DROP TABLE IF EXISTS ".$table_name;

	$wpdb->query($drop_sql);
}
register_deactivation_hook(__FILE__, 'helloworld_uninstall');

function helloworld_get_random_snippet() {
	global $wpdb, $helloworld_table_name;
	
	$table_name = $wpdb->prefix.$helloworld_table_name;

	$count_sql = "SELECT COUNT(*) FROM `$table_name`";
	
	$num_snippets = $wpdb->get_var($count_sql);

	$which = rand(0, $num_snippets);

	$code = $wpdb->get_var("SELECT code FROM $table_name WHERE id = $which"); 

	return $code;
}
add_shortcode('helloworldsnippet','helloworld_get_random_snippet');

function helloworld_post_shortcode_pre($atts) {
	return "<pre>".helloworld_get_random_snippet()."</pre>";
}
add_shortcode('helloworldsnippet_pre','helloworld_post_shortcode_pre');

?>
