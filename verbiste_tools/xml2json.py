#!/usr/bin/env python3
"""
XML to JSON converter for Verbiste data files using XSLT.
This script converts Verbiste XML files to JSON format with a cleaner structure.
"""

import argparse
import json
import os
import sys
from typing import Any, Dict, Optional

from lxml import etree


def create_verbs_transform() -> etree.XSLT:
    """Create an XSLT transform for verbs-fr.xml."""
    xslt = """
    <xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
        <xsl:output method="text" encoding="utf-8"/>
        
        <xsl:template match="/">
            <xsl:text>{"verbs": {</xsl:text>
            <xsl:apply-templates select="verbs/v"/>
            <xsl:text>}}</xsl:text>
        </xsl:template>
        
        <xsl:template match="v">
            <xsl:text>"</xsl:text>
            <xsl:value-of select="@i"/>
            <xsl:text>": {"template": "</xsl:text>
            <xsl:value-of select="@t"/>
            <xsl:text>"}</xsl:text>
            <xsl:if test="position() != last()">
                <xsl:text>,</xsl:text>
            </xsl:if>
        </xsl:template>
    </xsl:stylesheet>
    """
    return etree.XSLT(etree.XML(xslt))


def create_conjugation_transform() -> etree.XSLT:
    """Create an XSLT transform for conjugation-fr.xml."""
    xslt = """
    <xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
        <xsl:output method="text" encoding="utf-8"/>
        
        <xsl:template match="/">
            <xsl:text>{"templates": {</xsl:text>
            <xsl:apply-templates select="conjugation/template"/>
            <xsl:text>}}</xsl:text>
        </xsl:template>
        
        <xsl:template match="template">
            <xsl:text>"</xsl:text>
            <xsl:value-of select="@name"/>
            <xsl:text>": {</xsl:text>
            
            <!-- Infinitive -->
            <xsl:text>"infinitive": "</xsl:text>
            <xsl:value-of select="infinitive"/>
            <xsl:text>",</xsl:text>
            
            <!-- Tenses -->
            <xsl:text>"tenses": {</xsl:text>
            <xsl:apply-templates select="tenses/tense"/>
            <xsl:text>}</xsl:text>
            
            <xsl:text>}</xsl:text>
            <xsl:if test="position() != last()">
                <xsl:text>,</xsl:text>
            </xsl:if>
        </xsl:template>
        
        <xsl:template match="tense">
            <xsl:text>"</xsl:text>
            <xsl:value-of select="@name"/>
            <xsl:text>": {</xsl:text>
            
            <xsl:text>"p1s": "</xsl:text><xsl:value-of select="p1s"/><xsl:text>",</xsl:text>
            <xsl:text>"p2s": "</xsl:text><xsl:value-of select="p2s"/><xsl:text>",</xsl:text>
            <xsl:text>"p3s": "</xsl:text><xsl:value-of select="p3s"/><xsl:text>",</xsl:text>
            <xsl:text>"p1p": "</xsl:text><xsl:value-of select="p1p"/><xsl:text>",</xsl:text>
            <xsl:text>"p2p": "</xsl:text><xsl:value-of select="p2p"/><xsl:text>",</xsl:text>
            <xsl:text>"p3p": "</xsl:text><xsl:value-of select="p3p"/><xsl:text>"</xsl:text>
            
            <xsl:text>}</xsl:text>
            <xsl:if test="position() != last()">
                <xsl:text>,</xsl:text>
            </xsl:if>
        </xsl:template>
    </xsl:stylesheet>
    """
    return etree.XSLT(etree.XML(xslt))


def prettify_json(json_str: str) -> str:
    """Prettify JSON string."""
    try:
        data = json.loads(json_str)
        return json.dumps(data, ensure_ascii=False, indent=2)
    except json.JSONDecodeError:
        return json_str


def convert_file(input_file: str, output_file: str) -> bool:
    """Convert an XML file to JSON using XSLT."""
    try:
        # Determine which transform to use based on filename
        basename = os.path.basename(input_file)
        if "verbs" in basename:
            transform = create_verbs_transform()
        elif "conjugation" in basename:
            transform = create_conjugation_transform()
        else:
            print(f"Unknown file type: {basename}", file=sys.stderr)
            return False
        
        # Parse and transform XML
        xml_doc = etree.parse(input_file)
        result = str(transform(xml_doc))
        
        # Prettify and write JSON
        pretty_json = prettify_json(result)
        with open(output_file, 'w', encoding='utf-8') as f:
            f.write(pretty_json)
        
        print(f"Converted {input_file} to {output_file}")
        return True
    except Exception as e:
        print(f"Error converting {input_file}: {e}", file=sys.stderr)
        return False


def main() -> int:
    """Main entry point."""
    parser = argparse.ArgumentParser(description='Convert Verbiste XML files to JSON using XSLT')
    parser.add_argument('input', help='Input XML file or directory')
    parser.add_argument('output', help='Output JSON file or directory')
    parser.add_argument('--recursive', '-r', action='store_true', help='Process directories recursively')
    
    args = parser.parse_args()
    
    # Handle directory conversion
    if os.path.isdir(args.input):
        if not os.path.exists(args.output):
            os.makedirs(args.output)
        
        success = True
        for filename in os.listdir(args.input):
            if filename.endswith('.xml'):
                input_path = os.path.join(args.input, filename)
                output_path = os.path.join(args.output, filename.replace('.xml', '.json'))
                if not convert_file(input_path, output_path):
                    success = False
        
        return 0 if success else 1
    
    # Handle single file conversion
    else:
        if not args.input.endswith('.xml'):
            print("Input file must be an XML file", file=sys.stderr)
            return 1
        
        if not args.output.endswith('.json'):
            if os.path.isdir(args.output):
                args.output = os.path.join(args.output, os.path.basename(args.input).replace('.xml', '.json'))
            else:
                args.output = args.output + '.json'
        
        return 0 if convert_file(args.input, args.output) else 1


if __name__ == '__main__':
    sys.exit(main())