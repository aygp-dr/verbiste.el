#!/usr/bin/env python3
"""
Generate embeddings for French verbs using Ollama.
This script reads a list of French verbs and generates embeddings for each
using the Ollama API with the specified model.
"""

import json
import time
import os
import click
from tqdm import tqdm
import ollama

@click.command()
@click.argument('input_file', type=click.Path(exists=True))
@click.argument('output_file', type=click.Path())
@click.option('--model', default='nomic-embed-text', help='Ollama model to use for embeddings')
@click.option('--delay', default=0.1, help='Delay between API calls (seconds)')
@click.option('--batch-size', default=50, help='Number of verbs to process before saving progress')
def generate_embeddings(input_file, output_file, model, delay, batch_size):
    """Generate embeddings for French verbs and save them to a JSON file.
    
    Args:
        input_file: Path to the text file containing one French verb per line
        output_file: Path to save the JSON file with embeddings
        model: Ollama model to use for embeddings
        delay: Delay between API calls (seconds)
        batch_size: Number of verbs to process before saving progress
    """
    # Create output directory if it doesn't exist
    os.makedirs(os.path.dirname(output_file) or '.', exist_ok=True)
    
    # Read verb list
    with open(input_file, 'r', encoding='utf-8') as file:
        verbs = [line.strip() for line in file if line.strip()]
    
    click.echo(f"Found {len(verbs)} verbs to process")
    
    # Check if output file already exists to resume progress
    embeddings = {}
    if os.path.exists(output_file):
        try:
            with open(output_file, 'r', encoding='utf-8') as file:
                embeddings = json.load(file)
            click.echo(f"Loaded {len(embeddings)} existing embeddings from {output_file}")
            
            # Filter verbs that already have embeddings
            verbs = [verb for verb in verbs if verb not in embeddings]
            click.echo(f"Remaining verbs to process: {len(verbs)}")
        except json.JSONDecodeError:
            click.echo(f"Error loading existing embeddings from {output_file}, starting fresh")
    
    # Process verbs and get embeddings
    count = 0
    for verb in tqdm(verbs, desc="Processing verbs"):
        try:
            # Get embedding using ollama library
            response = ollama.embeddings(model=model, prompt=verb)
            
            # Extract the embedding
            embedding = response.get("embedding")
            if embedding:
                embeddings[verb] = embedding
            else:
                click.echo(f"Warning: No embedding returned for '{verb}'")
            
            # Save progress periodically
            count += 1
            if count % batch_size == 0:
                with open(output_file, 'w', encoding='utf-8') as file:
                    json.dump(embeddings, file, ensure_ascii=False)
                click.echo(f"Saved progress: {len(embeddings)} embeddings")
                
            time.sleep(delay)  # Avoid overwhelming the API
            
        except Exception as e:
            click.echo(f"Error processing '{verb}': {str(e)}")
    
    # Final save
    with open(output_file, 'w', encoding='utf-8') as file:
        json.dump(embeddings, file, ensure_ascii=False)
    
    # Print summary
    click.echo(f"Successfully created embeddings for {len(embeddings)} verbs")
    click.echo(f"Saved to {os.path.abspath(output_file)}")
    
    # Show a sample of the first verb embedding
    if embeddings:
        first_verb = next(iter(embeddings))
        embedding_length = len(embeddings[first_verb])
        click.echo(f"\nSample embedding for '{first_verb}':")
        click.echo(f"Dimension: {embedding_length}")
        click.echo(f"First 5 values: {embeddings[first_verb][:5]}")

if __name__ == "__main__":
    generate_embeddings()
