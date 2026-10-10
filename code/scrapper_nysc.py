import pandas as pd
from selenium import webdriver
from selenium.webdriver.support.ui import WebDriverWait, Select
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.common.by import By
from selenium.common.exceptions import TimeoutException, NoSuchElementException, StaleElementReferenceException
import time
import os

# --- CONFIGURATION ---
INPUT_CSV = "nysc_school_list.csv"
OUTPUT_CSV = "nysc_students_datatest.csv"
URL = "https://portal.nysc.org.ng/nysc1/CheckInstitutionCoursesPCMs.aspx"

# --- 1. RESUME LOGIC ---
completed_courses = set()

if os.path.exists(OUTPUT_CSV):
    print(f"Found existing data file: {OUTPUT_CSV}")
    try:
        # Read in chunks to avoid memory issues with large files
        for chunk in pd.read_csv(OUTPUT_CSV, chunksize=5000):
            for _, row in chunk.iterrows():
                # Signature: Batch + School + Course
                signature = (str(row['Batch']), str(row['Institution']), str(row['Course']))
                completed_courses.add(signature)
        print(f"Resuming... {len(completed_courses)} course-batches already scraped.")
    except Exception as e:
        print(f"Could not read existing file: {e}")
else:
    print("Starting fresh student extraction.")

# --- LOAD INPUT ---
try:
    df = pd.read_csv(INPUT_CSV)
    df.columns = [c.strip().lower() for c in df.columns] 
    df = df[df['inst_type'].astype(str).str.contains("University", case=False, na=False)]
    print(f"Loaded {len(df)} university records.")
except FileNotFoundError:
    print("Error: Input CSV not found.")
    exit()

# ... (After loading INPUT_CSV into df) ...

# --- NEW FAST FORWARD LOGIC ---
if os.path.exists(OUTPUT_CSV) and len(completed_courses) > 0:
    try:
        # 1. Peek at the last row of your saved data
        # We read just the last few rows to find the last school processed
        import collections
        
        # Read the file in reverse or just tail it (efficient for large files)
        # Using pandas tail is easiest
        last_chunk = pd.read_csv(OUTPUT_CSV).tail(1)
        
        if not last_chunk.empty:
            last_school = last_chunk.iloc[0]['Institution']
            last_batch = last_chunk.iloc[0]['Batch']
            
            print(f"\n--- FAST FORWARD ---")
            print(f"Last saved school found: {last_school} ({last_batch})")
            
            # 2. Find where this school is in your Input List
            # We look for the index where School AND Batch match
            match = df[(df['school'] == last_school) & (df['batch'] == last_batch)]
            
            if not match.empty:
                start_index = match.index[0]
                print(f"Jumping ahead to row index {start_index}...")
                
                # 3. Slice the dataframe to start from this school
                # We start AT this school (in case it wasn't finished)
                df = df.loc[start_index:]
                print(f"Resuming processing from {len(df)} remaining schools.")
            else:
                print("Could not match last school to input list. Starting from top.")
                
    except Exception as e:
        print(f"Fast forward failed (safe resume will be used): {e}")

# ... (Continue to Browser Setup) ...


# --- BROWSER SETUP ---
def get_driver():
    options = webdriver.ChromeOptions()
    # options.add_argument("--headless") # Keep OFF to monitor
    options.add_argument("--disable-blink-features=AutomationControlled") 
    options.add_argument("--no-sandbox")
    options.add_argument("--disable-dev-shm-usage")
    return webdriver.Chrome(options=options)

def smart_select(driver, wait, element_id, target_text):
    try:
        wait.until(EC.presence_of_element_located((By.ID, element_id)))
        element = driver.find_element(By.ID, element_id)
        driver.execute_script("arguments[0].scrollIntoView(true);", element)
        select = Select(element)
        clean_target = str(target_text).strip().lower()
        
        for opt in select.options:
            clean_opt = opt.text.strip().lower()
            if clean_target == clean_opt or (clean_target in clean_opt and len(clean_target) > 5):
                select.select_by_visible_text(opt.text)
                return True
        return False
    except Exception:
        return False

# --- THE OPTIMIZED SCRAPER FUNCTION ---
def scrape_table_pages(driver, wait, batch, inst_type, school, course):
    """Scrapes table using 'Show 100' and 'Next' link logic"""
    students = []
    
    # 1. Click Extract Button
    try:
        extract_btn = wait.until(EC.element_to_be_clickable((By.ID, "ctl00_ContentPlaceHolder1_btnExtract")))
        extract_btn.click()
    except TimeoutException:
        print(" -> Extract button not found.")
        return []

    # 2. Wait for Table & OPTIMIZE VIEW (Show 100 rows)
    try:
        # Wait for table wrapper or message
        wait.until(lambda d: d.find_elements(By.ID, "GdvPCMCourses_wrapper") or d.find_elements(By.ID, "ctl00_ContentPlaceHolder1_lblmsg"))
        
        if "No Record" in driver.page_source:
             print(" -> [No Students Found]")
             return []

        # Locate the 'Show entries' dropdown and set to 100
        length_dd = driver.find_element(By.NAME, "GdvPCMCourses_length")
        select_length = Select(length_dd)
        select_length.select_by_value('100')
        
        # Wait a moment for the table to reload with 100 rows
        time.sleep(3)
             
    except (TimeoutException, NoSuchElementException):
        print(" -> Timeout or No Table found.")
        return []

    # 3. Pagination Loop (Using 'Next' Link)
    while True:
        try:
            # A. Scrape the current visible rows
            table = driver.find_element(By.ID, "GdvPCMCourses")
            rows = table.find_elements(By.TAG_NAME, "tr")
            
            # Skip header (row 0)
            for row in rows[1:]:
                cols = row.find_elements(By.TAG_NAME, "td")
                if len(cols) >= 6:
                    students.append({
                        "Batch": batch,
                        "Institution Type": inst_type,
                        "Institution": school,
                        "Course": course,
                        "S/No": cols[0].text,
                        "Surname": cols[1].text,
                        "OtherNames": cols[2].text,
                        "Gender": cols[3].text,
                        "AccreditedCourse": cols[4].text,
                        "Status": cols[5].text
                    })
            
            # B. Check for "Next" Button
            try:
                next_link = driver.find_element(By.LINK_TEXT, "Next")
                
                # Check if it is disabled
                if "disabled" in next_link.get_attribute("class"):
                    break 
                
                # Click and wait for update
                next_link.click()
                time.sleep(2) 
                
            except NoSuchElementException:
                # If "Next" link doesn't exist, we are on the only page
                break
                
        except StaleElementReferenceException:
            continue # DOM updated too fast, retry loop
        except Exception as e:
            print(f" -> Error on page: {e}")
            break
            
    return students

# --- MAIN EXECUTION LOOP ---
driver = get_driver()
wait = WebDriverWait(driver, 15)

print("Starting Final Extraction with Progress Tracking...")

for index, row in df.iterrows():
    
    # Restart browser periodically
    if index > 0 and index % 20 == 0:
        print("\n--- RESTARTING BROWSER ---\n")
        driver.quit()
        time.sleep(5)
        driver = get_driver()
        wait = WebDriverWait(driver, 15)

    batch = row['batch']
    inst_type = row['inst_type']
    school_name = row['school']
    
    # Retry Loop for School Access
    max_retries = 3
    for attempt in range(max_retries):
        try:
            print(f"[{index+1}] {school_name}...", end=" ")
            
            driver.get(URL)
            
            # Bad Page Check
            try:
                wait.until(EC.presence_of_element_located((By.ID, "ctl00_ContentPlaceHolder1_cbo1stMostInstitutionType")))
            except TimeoutException:
                print(" -> Page Dead. Refreshing...")
                driver.refresh()
                time.sleep(5)
                continue

            # Select Dropdowns
            if not smart_select(driver, wait, "ctl00_ContentPlaceHolder1_cboBatch", batch):
                print(" -> Failed Batch")
                break 
            if not smart_select(driver, wait, "ctl00_ContentPlaceHolder1_cbo1stMostInstitutionType", inst_type):
                print(" -> Failed Type")
                break
            
            time.sleep(2) 
            school_dd = driver.find_element(By.ID, "ctl00_ContentPlaceHolder1_cboFirstInstitution")
            if len(Select(school_dd).options) <= 1:
                 print(" -> School list empty.")
                 continue 
            if not smart_select(driver, wait, "ctl00_ContentPlaceHolder1_cboFirstInstitution", school_name):
                print(" -> School Name Mismatch.")
                break 

            # --- GET COURSE LIST ---
            course_dd_id = "ctl00_ContentPlaceHolder1_cboCourses"
            course_select = Select(driver.find_element(By.ID, course_dd_id))
            
            if len(course_select.options) <= 1:
                time.sleep(3)
                course_select = Select(driver.find_element(By.ID, course_dd_id))
            
            all_courses = [opt.text for opt in course_select.options if "Select" not in opt.text]
            total_courses = len(all_courses)
            print(f"Found {total_courses} courses.")

            # --- INNER LOOP: SCRAPE EACH COURSE ---
            # Added enumerate to track progress (e.g., 1/20, 2/20)
            for i, course in enumerate(all_courses, 1):
                
                signature = (str(batch), str(school_name), str(course))
                if signature in completed_courses:
                    # Optional: Print skipped to show progress on resume
                    # print(f"    -> [{i}/{total_courses}] {course[:15]}... (Skipped)")
                    continue

                print(f"    -> [{i}/{total_courses}] {course[:30]}...", end=" ")
                
                # Re-select course (Page refresh resets it)
                if not smart_select(driver, wait, course_dd_id, course):
                    print("Select Failed.")
                    continue
                
                # --- CALL THE OPTIMIZED SCRAPER ---
                students_data = scrape_table_pages(driver, wait, batch, inst_type, school_name, course)
                
                if students_data:
                    df_out = pd.DataFrame(students_data)
                    write_header = not os.path.exists(OUTPUT_CSV)
                    df_out.to_csv(OUTPUT_CSV, mode='a', header=write_header, index=False)
                    print(f"Saved {len(students_data)}.")
                else:
                    print("Empty.")

                completed_courses.add(signature)

            break # School done

        except Exception as e:
            print(f" -> Error: {str(e)[:50]}. Retrying School...")
            time.sleep(5)

driver.quit()
print("Done.")