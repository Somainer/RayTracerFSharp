using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using OpenTK.Graphics;
using OpenTK.Graphics.OpenGL;
using OpenTK.Mathematics;
using OpenTK.Windowing.Common;
using OpenTK.Windowing.Desktop;
using OpenTK.Windowing.GraphicsLibraryFramework;
using RayTracer.Colors;

namespace RayTracer
{
    public class Window : GameWindow
    {
        private Scene Scene;
        private Color[,] buffer;
        private byte[] pixels;
        private Shader _shader;
        float[] vertices = {
            // Position         Texture coordinates
             1.0f,  1.0f, 0.0f, 1.0f, 1.0f, // top right
             1.0f, -1.0f, 0.0f, 1.0f, 0.0f, // bottom right
            -1.0f, -1.0f, 0.0f, 0.0f, 0.0f, // bottom left
            -1.0f,  1.0f, 0.0f, 0.0f, 1.0f  // top left
        };
        uint[] indices = {
            0, 1, 3, // first triangle
            1, 2, 3  // second triangle
        };
        private bool isDone = false;

        private int vertexArray;

        private int backgroundTexture;

        private Action stopRendering;
        private Vec3d[,] colorBuffer;

#pragma warning disable CS8618 // _shader will always be constructed in OnLoad.
        private Window(GameWindowSettings gameWindowSettings, NativeWindowSettings nativeWindowSettings, Scene scene)
#pragma warning restore CS8618
            : base(gameWindowSettings, nativeWindowSettings)
        {
            Scene = scene;
            buffer = new Color[scene.height, scene.width];
            pixels = new byte[buffer.Length * 4];
        }

        public static Window ForScene(Scene scene)
        {
            var settings = new NativeWindowSettings()
            {
                Size = new Vector2i(scene.width, scene.height),
                Title = "Ray Tracer App",
                Flags = ContextFlags.ForwardCompatible
            };

            return new Window(GameWindowSettings.Default, settings, scene);
        }

        protected override void OnLoad()
        {
            // var background = Scene.background;
            //GL.ClearColor((float) background.x, (float) background.y, (float) background.z, 1.0f);
            GL.Viewport(0, 0, Scene.width, Scene.height);

            vertexArray = GL.GenVertexArray();
            GL.BindVertexArray(vertexArray);
            
            var vbo = GL.GenBuffer();
            var ebo = GL.GenBuffer();

            GL.BindBuffer(BufferTarget.ArrayBuffer, vbo);
            GL.BufferData(BufferTarget.ArrayBuffer, vertices.Length * sizeof(float), vertices, BufferUsageHint.StaticDraw);

            GL.BindBuffer(BufferTarget.ElementArrayBuffer, ebo);
            GL.BufferData(BufferTarget.ElementArrayBuffer, indices.Length * sizeof(uint), indices, BufferUsageHint.StaticDraw);

            _shader = new Shader("Shaders/shader.vert", "Shaders/shader.frag");
            _shader.Use();
            var vertexLocation = _shader.GetAttribLocation("aPosition");
            GL.EnableVertexAttribArray(vertexLocation);
            GL.VertexAttribPointer(vertexLocation, 3, VertexAttribPointerType.Float, false, 5 * sizeof(float), 0);
            var texCoordLocation = _shader.GetAttribLocation("aTexCoord");
            GL.EnableVertexAttribArray(texCoordLocation);
            GL.VertexAttribPointer(texCoordLocation, 2, VertexAttribPointerType.Float, false, 5 * sizeof(float), 3 * sizeof(float));

            Task.Run(() =>
            {
                Console.WriteLine($"Rendering {Scene.width}x{Scene.height} image with spp = {Scene.spp}.");
                Scene.EvolvingRendering(RenderCallback, (_, color, action) =>
                {
                    stopRendering = action;
                    colorBuffer = color;
                });
                Scene.RenderWithCallback(RenderCallback, MarkIsDone);
            });
            base.OnLoad();
        }

        public void RenderCallback(int i, int j, Color color)
        {
            buffer[i, j] = color;
        }

        public void MarkIsDone(Vec3d[] pixels)
        {
            isDone = true;
            new RenderResult(Scene.height, Scene.width, pixels, Scene.spp).WriteToPPMFile("result.ppm");
        }

        // This function runs on every update frame.
        protected override void OnUpdateFrame(FrameEventArgs e)
        {
            // Check if the Escape button is currently being pressed.
            if (KeyboardState.IsKeyDown(Keys.Escape))
            {
                // If it is, close the window.
                Close();
            }

            if (KeyboardState.IsKeyDown(Keys.Enter))
            {
                stopRendering?.Invoke();
                MarkIsDone(colorBuffer.Cast<Vec3d>().ToArray());
            }

            base.OnUpdateFrame(e);
        }

        protected override unsafe void OnRenderFrame(FrameEventArgs args)
        {
            GL.Clear(ClearBufferMask.ColorBufferBit | ClearBufferMask.DepthBufferBit);
            //GL.MatrixMode(MatrixMode.Modelview);
            //GL.LoadIdentity();
            if (!isDone)
            {
                int p = 0;
                for (var i = Scene.height - 1; i >= 0; --i)
                {
                    for (var j = 0; j < Scene.width; ++j)
                    {
                        var color = buffer[i, j];
                        pixels[p++] = color.r;
                        pixels[p++] = color.g;
                        pixels[p++] = color.b;
                        pixels[p++] = color.a;
                    }
                }
            
                GL.DeleteTexture(backgroundTexture);
                backgroundTexture = GL.GenTexture();

                GL.ActiveTexture(TextureUnit.Texture0);
                GL.BindTexture(TextureTarget.Texture2D, backgroundTexture);
                GL.TexImage2D(TextureTarget.Texture2D, 0, PixelInternalFormat.Rgba, Scene.width, Scene.height, 0, PixelFormat.Rgba, PixelType.UnsignedByte, pixels);
                GL.TexParameter(TextureTarget.Texture2D, TextureParameterName.TextureWrapS, (int)TextureWrapMode.ClampToBorder);
                GL.TexParameter(TextureTarget.Texture2D, TextureParameterName.TextureWrapT, (int)TextureWrapMode.ClampToBorder);
                GL.TexParameter(TextureTarget.Texture2D, TextureParameterName.TextureMinFilter, (int)TextureMinFilter.Linear);
                GL.TexParameter(TextureTarget.Texture2D, TextureParameterName.TextureMagFilter, (int)TextureMagFilter.Linear);
                var borderColor = stackalloc float[] { 1.0f, 1.0f, 1.0f, 1.0f };
                GL.TexParameter(TextureTarget.Texture2D, TextureParameterName.TextureBorder, borderColor);
                //_shader.SetInt("texture0", 0);
                GL.GenerateMipmap(GenerateMipmapTarget.Texture2D);
            }
        
            _shader.Use();
            GL.BindVertexArray(vertexArray);
            GL.DrawElements(PrimitiveType.Triangles, indices.Length, DrawElementsType.UnsignedInt, 0);
        
            SwapBuffers();
            
            base.OnRenderFrame(args);
        }

        protected override void OnResize(ResizeEventArgs e)
        {
            base.OnResize(e);

            GL.Viewport(0, 0, Size.X, Size.Y);
        }
    }
}
