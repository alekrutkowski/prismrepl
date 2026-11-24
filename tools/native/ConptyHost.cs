// Test infrastructure only. Production prismrepl uses neither .NET nor C#.
// API lifetime follows Microsoft's "Creating a Pseudoconsole session" guide.
using System;
using System.ComponentModel;
using System.IO;
using System.Runtime.InteropServices;
using System.Text;
using System.Threading;

namespace PrismRepl.Tests {
    public sealed class ConptyHost : IDisposable {
        [StructLayout(LayoutKind.Sequential)]
        private struct Coord {
            public short X, Y;
            public Coord(short x, short y) { X = x; Y = y; }
        }
        [StructLayout(LayoutKind.Sequential, CharSet = CharSet.Unicode)]
        private struct StartupInfo {
            public int cb;
            public string lpReserved, lpDesktop, lpTitle;
            public int dwX, dwY, dwXSize, dwYSize, dwXCountChars, dwYCountChars;
            public int dwFillAttribute, dwFlags;
            public short wShowWindow, cbReserved2;
            public IntPtr lpReserved2, hStdInput, hStdOutput, hStdError;
        }
        [StructLayout(LayoutKind.Sequential)]
        private struct StartupInfoEx {
            public StartupInfo StartupInfo;
            public IntPtr lpAttributeList;
        }
        [StructLayout(LayoutKind.Sequential)]
        private struct ProcessInfo {
            public IntPtr hProcess, hThread;
            public int dwProcessId, dwThreadId;
        }

        [DllImport("kernel32.dll", SetLastError = true)]
        private static extern bool CreatePipe(out IntPtr read, out IntPtr write,
                                              IntPtr attributes, int size);
        [DllImport("kernel32.dll")]
        private static extern int CreatePseudoConsole(Coord size, IntPtr input,
                                                       IntPtr output, uint flags,
                                                       out IntPtr console);
        [DllImport("kernel32.dll")]
        private static extern int ResizePseudoConsole(IntPtr console, Coord size);
        [DllImport("kernel32.dll")]
        private static extern void ClosePseudoConsole(IntPtr console);
        [DllImport("kernel32.dll", SetLastError = true)]
        private static extern bool InitializeProcThreadAttributeList(IntPtr list,
            int count, int flags, ref IntPtr bytes);
        [DllImport("kernel32.dll", SetLastError = true)]
        private static extern bool UpdateProcThreadAttribute(IntPtr list, uint flags,
            IntPtr attribute, IntPtr value, IntPtr size, IntPtr previous, IntPtr returned);
        [DllImport("kernel32.dll")]
        private static extern void DeleteProcThreadAttributeList(IntPtr list);
        [DllImport("kernel32.dll", SetLastError = true, CharSet = CharSet.Unicode)]
        private static extern bool CreateProcessW(string application, StringBuilder command,
            IntPtr processAttributes, IntPtr threadAttributes, bool inheritHandles,
            uint flags, IntPtr environment, string directory, ref StartupInfoEx startup,
            out ProcessInfo process);
        [DllImport("kernel32.dll", SetLastError = true)]
        private static extern bool ReadFile(IntPtr handle, byte[] buffer, int count,
                                             out int read, IntPtr overlapped);
        [DllImport("kernel32.dll", SetLastError = true)]
        private static extern bool WriteFile(IntPtr handle, byte[] buffer, int count,
                                              out int written, IntPtr overlapped);
        [DllImport("kernel32.dll", SetLastError = true)]
        private static extern bool GetExitCodeProcess(IntPtr process, out uint status);
        [DllImport("kernel32.dll", SetLastError = true)]
        private static extern bool TerminateProcess(IntPtr process, uint status);
        [DllImport("kernel32.dll")]
        private static extern uint WaitForSingleObject(IntPtr handle, uint milliseconds);
        [DllImport("kernel32.dll", SetLastError = true)]
        private static extern bool CloseHandle(IntPtr handle);

        private IntPtr inputWrite, outputRead, console, process;
        private readonly object outputLock = new object();
        private readonly MemoryStream output = new MemoryStream();
        private Thread reader;
        private bool disposed;
        private string readerError;

        public ConptyHost(string executable, string arguments, string directory,
                          short width, short height) {
            IntPtr inputRead = IntPtr.Zero, outputWrite = IntPtr.Zero;
            IntPtr attributes = IntPtr.Zero;
            bool attributesInitialized = false;
            try {
                Check(CreatePipe(out inputRead, out inputWrite, IntPtr.Zero, 0));
                Check(CreatePipe(out outputRead, out outputWrite, IntPtr.Zero, 0));
                Marshal.ThrowExceptionForHR(CreatePseudoConsole(new Coord(width, height),
                    inputRead, outputWrite, 0, out console));
                IntPtr bytes = IntPtr.Zero;
                InitializeProcThreadAttributeList(IntPtr.Zero, 1, 0, ref bytes);
                attributes = Marshal.AllocHGlobal(bytes);
                Check(InitializeProcThreadAttributeList(attributes, 1, 0, ref bytes));
                attributesInitialized = true;
                Check(UpdateProcThreadAttribute(attributes, 0, new IntPtr(0x00020016),
                    console, new IntPtr(IntPtr.Size), IntPtr.Zero, IntPtr.Zero));
                StartupInfoEx startup = new StartupInfoEx();
                startup.StartupInfo.cb = Marshal.SizeOf(typeof(StartupInfoEx));
                startup.lpAttributeList = attributes;
                // Drain output on a dedicated thread to avoid ConPTY pipe deadlocks.
                reader = new Thread(ReadOutput);
                reader.IsBackground = true;
                reader.Start();
                ProcessInfo child;
                Check(CreateProcessW(executable,
                    new StringBuilder("\"" + executable + "\" " + arguments),
                    IntPtr.Zero, IntPtr.Zero, false, 0x00080000 | 0x00000400,
                    IntPtr.Zero, directory, ref startup, out child));
                process = child.hProcess;
                CloseHandle(child.hThread);
            } catch {
                if (inputRead != IntPtr.Zero) { CloseHandle(inputRead); inputRead = IntPtr.Zero; }
                if (outputWrite != IntPtr.Zero) { CloseHandle(outputWrite); outputWrite = IntPtr.Zero; }
                Dispose();
                throw;
            } finally {
                if (attributesInitialized) DeleteProcThreadAttributeList(attributes);
                if (attributes != IntPtr.Zero) Marshal.FreeHGlobal(attributes);
                if (inputRead != IntPtr.Zero) CloseHandle(inputRead);
                if (outputWrite != IntPtr.Zero) CloseHandle(outputWrite);
            }
        }

        private static void Check(bool success) {
            if (!success) throw new Win32Exception(Marshal.GetLastWin32Error());
        }

        private void ReadOutput() {
            byte[] buffer = new byte[16384];
            int count;
            while (ReadFile(outputRead, buffer, buffer.Length, out count, IntPtr.Zero)
                   && count > 0) {
                lock (outputLock) {
                    if (output.Length + count > 64 * 1024 * 1024)
                        readerError = "ConPTY smoke-test output exceeded 64 MiB.";
                    else output.Write(buffer, 0, count);
                }
            }
        }

        public string Snapshot() {
            lock (outputLock) {
                if (readerError != null) throw new IOException(readerError);
                return Encoding.UTF8.GetString(output.ToArray());
            }
        }

        public void Send(string text) {
            byte[] bytes = Encoding.UTF8.GetBytes(text);
            int offset = 0;
            while (offset < bytes.Length) {
                byte[] remaining = new byte[bytes.Length - offset];
                Array.Copy(bytes, offset, remaining, 0, remaining.Length);
                int written;
                Check(WriteFile(inputWrite, remaining, remaining.Length,
                                out written, IntPtr.Zero));
                if (written == 0) throw new IOException("ConPTY input stopped accepting data.");
                offset += written;
            }
        }

        public void Resize(short width, short height) {
            Marshal.ThrowExceptionForHR(ResizePseudoConsole(console, new Coord(width, height)));
        }

        public bool HasExited {
            get { return process != IntPtr.Zero && WaitForSingleObject(process, 0) == 0; }
        }

        public uint ExitCode {
            get {
                uint status;
                Check(GetExitCodeProcess(process, out status));
                return status;
            }
        }

        public bool WaitForExit(uint milliseconds) {
            return WaitForSingleObject(process, milliseconds) == 0;
        }

        public void Dispose() {
            if (disposed) return;
            disposed = true;
            if (process != IntPtr.Zero && !HasExited) TerminateProcess(process, 1);
            if (inputWrite != IntPtr.Zero) { CloseHandle(inputWrite); inputWrite = IntPtr.Zero; }
            // Keep the reader alive while ClosePseudoConsole flushes pending output.
            if (console != IntPtr.Zero) { ClosePseudoConsole(console); console = IntPtr.Zero; }
            if (reader != null) reader.Join(5000);
            if (outputRead != IntPtr.Zero) { CloseHandle(outputRead); outputRead = IntPtr.Zero; }
            if (process != IntPtr.Zero) { CloseHandle(process); process = IntPtr.Zero; }
        }
    }
}
