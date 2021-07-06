import sys
import os

# Add the lib path to our sys path so jedi_language_server can find its references
SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(SCRIPT_DIR, "jedilsp"))


from jedi_language_server.cli import cli

sys.exit(cli())
