#!/usr/bin/env python3
"""
XML to JSON converter for Verbiste data files.
This script converts Verbiste XML files to JSON format for easier handling in Emacs 30.1+.
"""

import argparse
import json
import os
import sys
import xml.etree.ElementTree as ET


def xml_to_dict(element):
    """Convert an XML element to a dictionary."""
    result = {}
    
    # Add attributes
    if element.attrib:
        result.update(element.attrib)
    
    # Add children
    for child in element:
        child_dict = xml_to_dict(child)
        if child.tag in result:
            if type(result[child.tag]) is list:
                result[child.tag].append(child_dict)
            else:
                result[child.tag] = [result[child.tag], child_dict]
        else:
            result[child.tag] = child_dict
    
    # Add text content if it exists and is the only content
    if element.text and element.text.strip() and not result:
        return element.text.strip()
    
    return result


def convert_file(input_file, output_file):
    """Convert an XML file to JSON."""
    try:
        # Parse XML
        tree = ET.parse(input_file)
        root = tree.getroot()
        
        # Convert to dictionary
        data = {root.tag: xml_to_dict(root)}
        
        # Write JSON
        with open(output_file, 'w', encoding='utf-8') as f:
            json.dump(data, f, ensure_ascii=False, indent=2)
        
        print(f"Converted {input_file} to {output_file}")
        return True
    except Exception as e:
        print(f"Error converting {input_file}: {e}", file=sys.stderr)
        return False


def main():
    parser = argparse.ArgumentParser(description='Convert Verbiste XML files to JSON')
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