from flask import Flask, render_template, request
import subprocess
import os

app = Flask(__name__)

EXAMPLES_DIR = "examples"
RESULTS_DIR = "results"

# Ensure the directories exist
os.makedirs(EXAMPLES_DIR, exist_ok=True)
os.makedirs(RESULTS_DIR, exist_ok=True)

@app.route('/', methods=['GET', 'POST'])
def index():
    if request.method == 'POST':
        code = request.form['code']
        file_name = os.path.join(EXAMPLES_DIR, "submitted_code.cpp")
        
        with open(file_name, 'w') as code_file:
            code_file.write(code)
        
        # Run the Racket program
        try:
            subprocess.run(['racket', 'racket\highlighter.rkt'], check=True)
        except subprocess.CalledProcessError as e:
            return f"An error occurred while processing the file: {e}"
        
        result_file = os.path.join(RESULTS_DIR, "submitted_code.html")
        with open(result_file, 'r', encoding='utf-8') as f:
            html_content = f.read()
        
        return render_template('index.html', code=code, content=html_content)
    
    return render_template('index.html')

if __name__ == '__main__':
    app.run(debug=True)
