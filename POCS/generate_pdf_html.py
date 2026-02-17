
import os
import re

def generate_html():
    sow_md_path = 'Engineering - SOW v5.md'
    template_path = 'sow_to_pdf.html'
    output_path = 'sow_v5_to_pdf.html'

    # Read the markdown content
    with open(sow_md_path, 'r', encoding='utf-8') as f:
        md_content = f.read()

    # Read the template HTML
    with open(template_path, 'r', encoding='utf-8') as f:
        html_content = f.read()

    # Replace the markdown source
    # Regex to find the script tag with id="markdown-source"
    pattern = r'(<script type="text/plain" id="markdown-source">)([\s\S]*?)(</script>)'
    
    # We need to escape backslashes in md_content for regex substitution if we were using re.sub with a string
    # But it's safer to do string splitting/joining to avoid regex escaping issues with the content
    
    parts = re.split(pattern, html_content)
    if len(parts) >= 4:
        # parts[0] is before opening tag
        # parts[1] is opening tag
        # parts[2] is old content
        # parts[3] is closing tag
        # parts[4] is after closing tag
        
        # Construct new HTML
        new_html = parts[0] + parts[1] + '\n' + md_content + '\n' + parts[3] + parts[4]
        
        # Also update the filename in the JS
        new_html = new_html.replace("filename: 'Engineering_SOW_v4.pdf'", "filename: 'Engineering_SOW_v5.pdf'")
        
        with open(output_path, 'w', encoding='utf-8') as f:
            f.write(new_html)
        print(f"Successfully created {output_path}")
    else:
        print("Could not find the markdown-source script tag in the template.")

if __name__ == "__main__":
    generate_html()
